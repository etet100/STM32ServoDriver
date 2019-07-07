#include <string.h>
#include <math.h>
#include "defines.h"
#include "configuration.h"
#include "spid.h"
#include "usart.h"
#include "printf/printf.h"

volatile PID pidData;

void resetSPid()
{
	pidData.en = 1.0f;
	pidData.cmd = 0.0f;
	pidData.fb = 0.0f;
	pidData.fbint = 0.0f;

	pidData.stats_max_error = 0.0f;
	pidData.stats_max_output = 0.0f;
}

void initSPid()
{
	memset((void*) &pidData, sizeof(PID), 1);

	pidData.kp = 25.0f;
	pidData.ki = 0.0f;
	pidData.kd = 0.0f;

	pidData.kff0 = 0.0f;
	pidData.kff1 = 0.0f;

	pidData.ksd = 0.0f;
	pidData.ksdi = 0.0f;
	pidData.kdi = 0.0f;

	pidData.max_error = 1000.0f;
	pidData.max_output = PWM_MAX;
	pidData.fault_error = 1500.0f;
	pidData.step_multiplier = 1;

	resetSPid();
}

void calcSPid(float period)
{
	float max_output = pidData.max_output;	// - offset;
	float min_output = -pidData.max_output;	// - offset;
	float max_error = pidData.max_error;
	float fault_error = pidData.fault_error;

	float cmd = pidData.cmd;
	float cmd_d = (cmd - pidData.last_cmd) / period;
	float error = cmd - pidData.fb;
	if (fault_error > 0.0f && fabs(error) > fault_error) {
		pidData.en = 0.0f;
	}
	if (pidData.deadband > 0 && fabs(error) <= (float)pidData.deadband) {
		error = 0.0f;
	}
	if (max_error > 0.0f) {
		error = LIMIT(error, max_error);
	}
	float error_d = (error - pidData.last_error) / period;

	float output = 0.0f;
	output += cmd * pidData.kff0;    // feedforward 0
	output += cmd_d * pidData.kff1;  // feedforward 1
	output += error * pidData.kp;    // proportional
	output += error_d * pidData.kd;  // differential
	if (
		pidData.ksd != 0.0f &&
		ABS(error) > (max_output - min_output) / pidData.ksd * 0.001f
	) {
		pidData.error_sum += error_d / ABS(error) * pidData.ksd; // scalded differential
	}
	output = CLAMP(output, min_output, max_output);

	pidData.error_sum += error * pidData.ki * period;     // integrator
	pidData.error_sum += error_d * pidData.kdi * period; // differential integrator
	if (
		pidData.ksdi != 0.0f &&
		ABS(error) > (max_output - min_output) / pidData.ksdi * 0.001f
	) {
		pidData.error_sum += error_d / ABS(error) * pidData.ksdi * period; // scalded differential integrator
	}
	pidData.error_sum = CLAMP(
		pidData.error_sum,
		min_output - output,
		max_output - output
	);  // dynamic anti windup

	output += pidData.error_sum;

	if (pidData.en <= 0.0f) {
		output = 0.0f;
		pidData.error_sum = 0.0f;
	}

	if (output <= min_output || output >= max_output) {
		pidData.sat += period;
	} else {
		pidData.sat = 0.0f;
	}

	pidData.output = output;

	pidData.error = error;
	pidData.last_error = error;
	pidData.last_cmd = cmd;

	pidData.stats_max_error = MAX(ABS(error), pidData.stats_max_error);
	pidData.stats_max_output = MAX(ABS(output), pidData.stats_max_output);
}

void handleStatus()
{
	static int statusDivider = 0;
	if (++statusDivider == PID_UPDATE_RATE / 10) {
		char buf[100];
		sprintf(buf, "STA %d %.2f %d %d %.2f %.2f %d %d\n", // @suppress("Float formatting support")
			(int)pidData.error,
			pidData.error_sum,
			(int)pidData.cmd,
			(int)pidData.fb,
			pidData.output * 100.00f / PWM_MAX,
			pidData.sat,
			(int)pidData.stats_max_error,
			(int)pidData.stats_max_output
		);
		usart_sendStr(buf);

		statusDivider = 0;
	}
}

