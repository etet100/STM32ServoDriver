/*
 	bldc-drive Cheap and simple brushless DC motor driver designed for CNC applications using STM32 microcontroller.
	Copyright (C) 2015 Pekka Roivainen

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/


#include "configuration.h"
#include "spid.h"
#include "usart.h"
#include "defines.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

uint8_t servoMode = SERVO_MODE_NORMAL;
float servoSpeed = 0;

#define EADDR_IS_INITIALIZED 0x0001
#define EADDR_CONFIG_START 0x0002

extern volatile PID pidData;

void selectServoMode(int mode, float speed)
{
	servoMode = (uint8_t)mode;
	switch (mode) {
		case SERVO_MODE_NORMAL:
			break;
		case SERVO_MODE_VELOCITY:
			servoSpeed = CLAMP(
				speed / (float)PID_UPDATE_RATE,
				-3.0f,
				3.0f
			);
			break;
		case SERVO_MODE_PWM:
			servoSpeed = (float)MAX_DUTY * CLAMP(speed, -100.0f, 100.0f) / 100.0f;
			break;
	}


}

void writeConfig()
{
/*
	FLASH_Unlock();


	uint16_t i;
	int16_t *ptr = (int16_t *)&s;
	for(i=0; i<sizeof(servoConfig)/2;i++)
	{
		if(EE_WriteVariable(EADDR_CONFIG_START+i, *ptr)!=FLASH_COMPLETE)
		{
			usart_sendStr("Flash Error\n\r");
		}
		ptr++;
	}

	FLASH_Lock();
*/
}
void getConfig()
{
	/*
	FLASH_Unlock();

	uint16_t i;

	uint16_t *ptr;
	EE_Init();
	EE_ReadVariable(EADDR_IS_INITIALIZED,&i);
	if(i != 0x5254)
	{
		//empty or corrupted EEPROM detected: write default config
		//EE_Format();
		EE_WriteVariable(EADDR_IS_INITIALIZED, 0x5254);
		s.commutationMethod = commutationMethod_Encoder;
		s.inputMethod = inputMethod_stepDir;
		s.encoder_PPR = 2400;
		s.encoder_poles = 7;
		s.encoder_counts_per_step = 1;
		s.invert_dirstepena = 7;
		s.max_error = 1000;
		s.pid_Kp = 10;
		s.pid_Ki = 0;
		s.pid_Kd = 0;
		s.pid_FF1 = 0;
		s.pid_FF2 = 0;
		s.pid_deadband = 0;
		s.max_current = 1000;
		s.usart_baud = 1152;
		s.commutation_offset=0;
		writeConfig();
		return;

	}
	ptr = (uint16_t *)&s;
	for(i=0; i<sizeof(servoConfig)/2;i++)
	{

		if(EE_ReadVariable(EADDR_CONFIG_START+i, ptr)!=0)
			usart_sendStr("Flash Error\n\r");
		ptr++;
	}
	FLASH_Lock();
	return;
*/
}

bool setConfig(char* param, char* value)
{
	char* end;

	#define setConfigParamF(A, B) if (!strcmp(param, #A)) { B = strtof(value, &end); return true; }
	#define addConfigParamF(A, B) if (!strcmp(param, #A)) { B += strtof(value, &end); return true; }
	#define subConfigParamF(A, B) if (!strcmp(param, #A)) { B -= strtof(value, &end); return true; }
	#define setConfigParamI(A, B) if (!strcmp(param, #A)) { B = atoi(value); return true; }
	#define setConfigParamI2(A, B, C) if (!strcmp(param, #A)) { B = C; return true; }
	#define addConfigParamI(A, B) if (!strcmp(param, #A)) { B += atoi(value); return true; }
	#define subConfigParamI(A, B) if (!strcmp(param, #A)) { B -= atoi(value); return true; }

	setConfigParamF(P, pidData.kp);
	setConfigParamF(I, pidData.ki);
	setConfigParamF(D, pidData.kd);
	setConfigParamF(SD, pidData.ksd);
	setConfigParamF(SDI, pidData.ksdi);
	setConfigParamF(DI, pidData.kdi);
	setConfigParamF(F0, pidData.kff0);
	setConfigParamF(F1, pidData.kff1);
	setConfigParamI(ME, pidData.max_error);
	setConfigParamI(DB, pidData.deadband);
	setConfigParamF(FE, pidData.fault_error);
	setConfigParamI2(MO, pidData.max_output, ((PWM_MAX / 100) * atoi(value)));
//	setConfigParamI2(O2, pidData.min_output, ((PWM_MAX / 100) * atoi(value)));
//	setConfigParamI(O2, ((PWM_MAX / 100) * pidData.min_output));
	setConfigParamI(C, pidData.cmd);
	addConfigParamI(C+, pidData.cmd);
	subConfigParamI(C-, pidData.cmd);
	setConfigParamI(SM, pidData.step_multiplier);
	addConfigParamI(POS, pidData.cmd);

	return false;
}

void printConfiguration()
{
	char buf[20];

	#define sendUartConfigF(A, B) sprintf(buf, "GET " #A " %.3f\n", B); usart_sendStr(buf)
	#define sendUartConfigI(A, B) sprintf(buf, "GET " #A " %d\n", B); usart_sendStr(buf)

	sendUartConfigF(P, pidData.kp);
	sendUartConfigF(I, pidData.ki);
	sendUartConfigF(D, pidData.kd);
	sendUartConfigF(SD, pidData.ksd);
	sendUartConfigF(SDI, pidData.ksdi);
	sendUartConfigF(DI, pidData.kdi);
	sendUartConfigF(F0, pidData.kff0);
	sendUartConfigF(F1, pidData.kff1);
	sendUartConfigI(ME, (int)pidData.max_error);
	sendUartConfigI(MO, (int)(pidData.max_output / PWM_MAX * 100.0f));
	//sendUartConfigI(O2, (int)(pidData.min_output / PWM_MAX * 100.0f));
	sendUartConfigI(DB, pidData.deadband);
	sendUartConfigF(FE, pidData.fault_error);
	sendUartConfigI(SM, pidData.step_multiplier);

	usart_sendStr("GET OK\n");
}

void printStatus()
{
	char buf[20];

	#define sendUartStatusF(A, B) sprintf(buf, "STA " #A " %.3f\n", B); usart_sendStr(buf)
	#define sendUartStatusI(A, B) sprintf(buf, "STA " #A " %d\n", B); usart_sendStr(buf)
	#define sendUartStatusU(A, B) sprintf(buf, "STA " #A " %u\n", B); usart_sendStr(buf)

	sendUartStatusF(O, pidData.output);
	sendUartStatusI(C, (int)pidData.cmd);
	sendUartStatusI(F, (int)pidData.fb);
	//sendUartStatusI(FINT, pidData.fbint);
	sendUartStatusF(E, pidData.last_error);
	sendUartStatusF(EN, pidData.en);
	sendUartStatusF(S, pidData.error_sum);

	usart_sendStr("STA OK\n");
}
