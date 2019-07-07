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


#include "stm32f3xx_hal.h"
#include "stm32f3xx_hal_gpio.h"
#include "stm32f3xx_hal_uart.h"

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "usart.h"
#include "spid.h"
#include "sampling.h"
//#include "pwm.h"
#include "usbd_cdc_if.h"
#include "configuration.h"

char txbuffer[255];

uint8_t USB_TX_Buffer[USB_TX_BUFFER_SIZE];
uint32_t USB_TX_Buffer_Pos = 0;

//char recvbuffer[255];
//uint8_t recvctr;
DMA_InitTypeDef DMA_InitStructure;

extern UART_HandleTypeDef huart1;
extern USBD_HandleTypeDef hUsbDeviceFS;

void initUSART(uint16_t baud)
{
	/*
	recvctr=0;
	serial_stream_enabled=0;
	GPIO_InitTypeDef GPIO_InitStructure;
	USART_InitTypeDef USART_InitStructure;
	NVIC_InitTypeDef NVIC_InitStructure;


	if(USART == USART1)
	{
		RCC_APB2PeriphClockCmd(RCC_APB2Periph_USART1 | RCC_APB2Periph_AFIO |
			                        RCC_APB2Periph_GPIOA, ENABLE);
		GPIO_InitStructure.GPIO_Mode = GPIO_Mode_AF_PP;
		GPIO_InitStructure.GPIO_Pin =  GPIO_Pin_9;
		GPIO_InitStructure.GPIO_Speed = GPIO_Speed_50MHz;
		GPIO_Init(GPIOA, &GPIO_InitStructure);


		GPIO_InitStructure.GPIO_Mode = GPIO_Mode_IN_FLOATING;
		GPIO_InitStructure.GPIO_Pin = GPIO_Pin_10 ;
		GPIO_Init(GPIOA, &GPIO_InitStructure);

		NVIC_InitStructure.NVIC_IRQChannel = USART1_IRQn;
		NVIC_InitStructure.NVIC_IRQChannelPreemptionPriority = 0x01;
		NVIC_InitStructure.NVIC_IRQChannelSubPriority = 5;
		NVIC_InitStructure.NVIC_IRQChannelCmd = ENABLE;
		NVIC_Init(&NVIC_InitStructure);
	}

	else if(USART==USART3)
	{

		RCC_APB1PeriphClockCmd(RCC_APB1Periph_USART3, ENABLE);
		RCC_APB2PeriphClockCmd(RCC_APB2Periph_GPIOB,ENABLE);
		RCC_APB2PeriphClockCmd(RCC_APB2Periph_AFIO, ENABLE);
		GPIO_InitStructure.GPIO_Pin = GPIO_Pin_10;
		GPIO_InitStructure.GPIO_Speed = GPIO_Speed_50MHz;
		GPIO_InitStructure.GPIO_Mode = GPIO_Mode_AF_PP;
		GPIO_Init(GPIOB, &GPIO_InitStructure);

		GPIO_InitStructure.GPIO_Pin = GPIO_Pin_11;
		GPIO_InitStructure.GPIO_Mode = GPIO_Mode_IN_FLOATING;
		GPIO_Init(GPIOB, &GPIO_InitStructure);

		NVIC_InitStructure.NVIC_IRQChannel = USART3_IRQn;
		NVIC_InitStructure.NVIC_IRQChannelPreemptionPriority = 0x01;
		NVIC_InitStructure.NVIC_IRQChannelSubPriority = 5;
		NVIC_InitStructure.NVIC_IRQChannelCmd = ENABLE;
		NVIC_Init(&NVIC_InitStructure);
	}

	USART_InitStructure.USART_BaudRate = baud*100;
	USART_InitStructure.USART_WordLength = USART_WordLength_8b;
	USART_InitStructure.USART_StopBits = USART_StopBits_1;
	USART_InitStructure.USART_Parity = USART_Parity_No;
	USART_InitStructure.USART_HardwareFlowControl = USART_HardwareFlowControl_None;
	USART_InitStructure.USART_Mode = USART_Mode_Rx | USART_Mode_Tx;

	USART_Init(USART, &USART_InitStructure);

	USART_ITConfig(USART, USART_IT_RXNE, ENABLE);

	USART_Cmd(USART, ENABLE);

	//DMA for USART TX!
	RCC_AHBPeriphClockCmd(RCC_AHBPeriph_DMA1, ENABLE);

	DMA_DeInit(DMA1_Channel2);
	DMA_InitStructure.DMA_PeripheralBaseAddr = 0x40004804;
	DMA_InitStructure.DMA_MemoryBaseAddr = (uint32_t)txbuffer;
	DMA_InitStructure.DMA_DIR = DMA_DIR_PeripheralDST;
	DMA_InitStructure.DMA_BufferSize = 255;
	DMA_InitStructure.DMA_PeripheralInc = DMA_PeripheralInc_Disable;
	DMA_InitStructure.DMA_MemoryInc = DMA_MemoryInc_Enable;
	DMA_InitStructure.DMA_PeripheralDataSize = DMA_PeripheralDataSize_Byte;
	DMA_InitStructure.DMA_MemoryDataSize = DMA_MemoryDataSize_Byte;
	DMA_InitStructure.DMA_Mode = DMA_Mode_Normal;
	DMA_InitStructure.DMA_Priority = DMA_Priority_VeryHigh;
	DMA_InitStructure.DMA_M2M = DMA_M2M_Disable;
	DMA_Init(DMA1_Channel2, &DMA_InitStructure);

	USART_DMACmd(USART3, USART_DMAReq_Tx, ENABLE);

	//DMA_Cmd(DMA1_Channel2, ENABLE);
*/

}

void usart_startDMA(uint16_t len)
{
	/*
	DMA_DeInit(DMA1_Channel2);
	DMA_InitStructure.DMA_PeripheralBaseAddr = 0x40004804;
	DMA_InitStructure.DMA_MemoryBaseAddr = (uint32_t) txbuffer;
	DMA_InitStructure.DMA_BufferSize = len;
	DMA_InitStructure.DMA_PeripheralInc = DMA_PeripheralInc_Disable;
	DMA_InitStructure.DMA_MemoryInc = DMA_MemoryInc_Enable;
	DMA_InitStructure.DMA_PeripheralDataSize = DMA_PeripheralDataSize_Byte;
	DMA_Init(DMA1_Channel2, &DMA_InitStructure);
	DMA_Cmd(DMA1_Channel2, ENABLE);
	*/
}

void usart_sendChar(char chr)
{
	/*
	USART_SendData(USART, chr);
	while (USART_GetFlagStatus(USART, USART_FLAG_TXE) == RESET) {
	};
	*/
}

void usart_sendStr(char *str)
{
	USBD_CDC_HandleTypeDef *hcdc =
			(USBD_CDC_HandleTypeDef*) hUsbDeviceFS.pClassData;
	if (USB_TX_Buffer_Pos == 0 && hcdc->TxState == 0) {
		//	HAL_UART_Transmit(&huart1, str, strlen(str), 1234);
		CDC_Transmit_FS((uint8_t*)str, strlen(str));
	} else {
		int len = strlen(str);
		if (USB_TX_Buffer_Pos + len < USB_TX_BUFFER_SIZE) {
			strncpy((char *)(USB_TX_Buffer + USB_TX_Buffer_Pos), str, len);
			USB_TX_Buffer_Pos += len;
		}
	}
}

void usart_send_stream()
{
	//sprintf(txbuffer, "STR:%d;%d;%d;%d;%d;%d;%d;%d\r",(int)hall,(int)TIM4->CNT,(int)pid_requested_position,(int)pid_last_requested_position_delta,(int)position_error,(int)ADC_value,(int)TIM1->CCR1,(int)pid_integrated_error);
	//usart_sendStr(txbuffer);
}

void parseUsart(char recvbuffer[255], uint8_t recvctr)
{
	const char delimiters[] = " ";
/*
	if (strstr(recvbuffer, "STREAM") != NULL) {
		strtok(recvbuffer, delimiters); //first param
		char *value = strtok(NULL, delimiters);
		if (strstr(value, "START")) {
			serial_stream_enabled = 1;
			usart_send_stream();
		} else {
			serial_stream_enabled = 0;
		}
	} else*/
	if (strstr(recvbuffer, "SET") != NULL) {
		strtok(recvbuffer, delimiters); //first param

		char *param = strtok(NULL, delimiters);
		char *value = strtok(NULL, delimiters);
		if (param != NULL && setConfig(param, value)) {
			sprintf(txbuffer,
				"SET %s %s\n\r",
				param,
				value);
		} else {
			sprintf(txbuffer,
				"SET %s ERR\n\r",
				param);
		}
		usart_sendStr(txbuffer);
	} else
	if (strstr(recvbuffer, "POS") != NULL) {
		strtok(recvbuffer, delimiters); //first param

		char *param = strtok(NULL, delimiters);
		setConfig("POS", param);

	} else
	if (strstr(recvbuffer, "STA") != NULL) {
		printStatus();
	} else
	if (strstr(recvbuffer, "SAM") != NULL) {
		strtok(recvbuffer, delimiters); //first param

		char *param = strtok(NULL, delimiters);

		startStopSamplingStream(atoi(param));
	} else
	if (strstr(recvbuffer, "MOD") != NULL) {
		strtok(recvbuffer, delimiters); //first param

		char *mode = strtok(NULL, delimiters);
		char *speed = strtok(NULL, delimiters);

		selectServoMode(atoi(mode), atof(speed));
	} else
	if (strstr(recvbuffer, "TES") != NULL) {
		strtok(recvbuffer, delimiters); //first param
		char *steps = strtok(NULL, delimiters);
		char *skipSamples = strtok(NULL, delimiters);
		char *samples = strtok(NULL, delimiters);
		startSampling(
			atoi(steps),
			atoi(skipSamples),
			atoi(samples),
			SAMPLING_DIVISION
		);
	} else
		if (strstr(recvbuffer, "TES") != NULL) {
			strtok(recvbuffer, delimiters); //first param
			char *steps = strtok(NULL, delimiters);
			char *skipSamples = strtok(NULL, delimiters);
			char *samples = strtok(NULL, delimiters);
			startSampling(
				atoi(steps),
				atoi(skipSamples),
				atoi(samples),
				SAMPLING_DIVISION
			);
		} else
	if (strstr(recvbuffer, "RES") != NULL) {
		resetSPid();
	} else
	if (strstr(recvbuffer, "SAV") != NULL) {
		usart_sendStr("Saving:\n\r");
		printConfiguration();
		writeConfig(s);
		usart_sendStr("SAVE OK\n\r");
	} else
	if (strstr(recvbuffer, "GET") != NULL) {
		printConfiguration();
	}

	if (recvctr < 3) {
		/*
		uint16_t len = sprintf(txbuffer,
				"Count: %d, error: %d, max_error: %d\n\r",
				(int) TIM4->CNT,
				(int) position_error,
				(int) max_error);
		max_error = 0;
		usart_startDMA(len);*/
	}
}

void serialInterrupt()
{
	/*
	char in;
	if(USART_GetITStatus(USART3, USART_IT_RXNE) != RESET)
	{
		in = (char)USART_ReceiveData(USART3);
		recvbuffer[recvctr] = in;

		if(in=='\r')
		{
			parseUsart();
			recvctr=0;

		}
		else
		{
			//any received character will stop the stream.
			serial_stream_enabled=0;
			recvctr++;
		}

	}
	*/
}

void USART3_IRQHandler(void)
{
	serialInterrupt();
}

/*void USART1_IRQHandler(void)
{
	serialInterrupt();
}
*/
