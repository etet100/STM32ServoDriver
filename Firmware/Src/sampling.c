#include "main.h"
#include "sampling.h"
#include "printf/printf.h"
#include "spid.h"
#include "usart.h"
#include "defines.h"

bool sampling = false;
int samplingIndex;
int samplingDivider;
int samplingStopAt;
int samplingSteps;
int samplingDivision;

extern volatile PID pidData;

//#define SAMPLING_STOP_AT SAMPLING_DIVISION * SAMPLES_COUNT

void startSampling(int steps, int skipSamples, int samples, int division)
{
	usart_sendStr("SAM START\n");

	sampling = true;
	samplingIndex = -skipSamples;
	samplingDivision = division;
	samplingDivider = -division * skipSamples;
	samplingStopAt = division * samples;
	samplingSteps = steps;
}

void stopSampling()
{
	sampling = false;

	usart_sendStr("SAM OK\n");
}

void startStopSamplingStream(int samplesPerSecond)
{
	if (samplingInProgress()) {
		stopSampling();
	} else {
		startSampling(0, 0, 0,
				(int) (24000.0f / (float) (CLAMP(samplesPerSecond, 1, 1000))));
	}
}

inline bool samplingInProgress()
{
	return sampling;
}

inline void sample()
{
	if (!(++samplingDivider % samplingDivision)) {

		char buf[50];
		sprintf(buf, "SAM %03X %d %d %d %d\n", samplingIndex++,
				(int) pidData.error, (int) (pidData.output * 100.00f / PWM_MAX),
				(int) pidData.error_sum, (int) pidData.sat); // @suppress("Float formatting support")
		usart_sendStr(buf);

		if (samplingSteps && samplingStopAt) {
			if (!samplingIndex) {
				pidData.cmd += samplingSteps;
			}

			if (samplingDivider >= samplingStopAt) {
				stopSampling();
			}
		}
	}
}

inline void handleSampling()
{
	if (samplingInProgress()) {
		sample();
	}
}

