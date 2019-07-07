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


#ifndef CONFIGURATION_H_
#define CONFIGURATION_H_

#include <stdint.h>
#include <stdbool.h>

#define SERVO_MODE_NORMAL 1
#define SERVO_MODE_VELOCITY 2
#define SERVO_MODE_PWM 3

#define SERVO_MODE_NORMAL 1

#define COMMUNICATION_UART 1
#define COMMUNICATION_USB 2

#define COMMUNICATION_CHANNEL COMMUNICATION_USB

#define PID_UPDATE_RATE 24000

//PID timer period. Counter runs at 1MHz, so f_pid = 1e6/period. 250=4kHz, 125=8Khz, 63=16kHz..
#define PID_TIM_PERIOD 40

#define PID_CALCULATION_FREQUENCY 20000
#define PWM_SIGN_MAGNITUDE_MODE 1
#define MAX_INTEGRATION_ERROR 400000

#define MAX_DUTY 3000

#define inputMethod_stepDir 1
#define inputMethod_pwmVelocity 2
#define commutationMethod_HALL 1
#define commutationMethod_Encoder 2

#define is_ena_inverted (s.invert_dirstepena>>0)&1
#define is_step_inverted (s.invert_dirstepena>>1)&1
#define is_dir_inverted (s.invert_dirstepena>>2)&1

typedef struct  {
	volatile uint16_t inputMethod;
	volatile uint16_t commutationMethod;
	volatile uint16_t encoder_PPR;
	volatile uint16_t encoder_poles;
	volatile uint16_t max_error;
	volatile uint16_t invert_dirstepena;
	volatile int16_t encoder_counts_per_step;
	volatile uint16_t max_current;
	volatile int16_t pid_Kp;
	volatile int16_t pid_Ki;
	volatile int16_t pid_Kd;
	volatile int16_t pid_FF1;
	volatile int16_t pid_FF2;
	volatile uint16_t pid_deadband;
	volatile int16_t commutation_offset;

	volatile uint16_t usart_baud; //baud divided by 100 to fit to 16 bits! for example 115200 => 1152

} servoConfig;
void getConfig();
void selectServoMode(int mode, float speed);
bool setConfig(char* param, char* value);
void writeConfig();
void printConfiguration();
void printStatus();

#endif /* CONFIGURATION_H_ */
