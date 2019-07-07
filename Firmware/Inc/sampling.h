/*
 * sampling.h
 *
 *  Created on: May 19, 2019
 *      Author: a
 */

#ifndef SAMPLING_H_
#define SAMPLING_H_

#include <stdbool.h>

#define SAMPLING_DIVISION 12
#define SAMPLES_COUNT 500

void sample();
void startSampling(int steps, int skipSamples, int samples, int division);
void stopSampling();
bool samplingInProgress();
void handleSampling();
void startStopSamplingStream(int samplesPerSecond);

#endif /* SAMPLING_H_ */
