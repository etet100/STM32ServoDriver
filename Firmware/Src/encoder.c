
#include "main.h"
#include "encoder.h"
#include "spid.h"
#include <stdlib.h>

extern volatile PID pidData;

inline void updateEncoder()
{
	static uint16_t lastEncoder;
	uint16_t encoder = (uint16_t) TIM4->CNT;
	if (encoder != lastEncoder) {
		if (abs(lastEncoder - encoder) > 0x8000) {
			if (lastEncoder > encoder) {
				//wzrost przechodząć przez 0xFFFF
				float a = (float) (encoder + (0xFFFF + 1 - lastEncoder));
				int b = (encoder + (0xFFFF + 1 - lastEncoder));
				pidData.fb += a;
				pidData.fbint += b;
			} else {
				//zmalał przez 0
				float a = (float) (lastEncoder + (0xFFFF + 1 - encoder));
				int b = (lastEncoder + (0xFFFF + 1 - encoder));
				pidData.fb -= a;
				pidData.fbint -= b;
			}
		} else {
			pidData.fb += (float) (encoder - lastEncoder);
			pidData.fbint += (encoder - lastEncoder);
		}
		lastEncoder = encoder;
	}
}
