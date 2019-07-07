
#include "main.h"
#include "stm32f3xx_hal_uart.h"
#include "usart.h"
#include "usbd_cdc_if.h"
#include "communication.h"

UART_HandleTypeDef huart1;
DMA_HandleTypeDef hdma_usart1_rx;

#define LINE_BUFFER_SIZE          DMA_RX_BUFFER_SIZE * 2

uint8_t DMA_RX_Buffer[DMA_RX_BUFFER_SIZE];

uint8_t USB_RX_Buffer[USB_RX_BUFFER_SIZE];
uint32_t USB_RX_Buffer_Pos = 0;

extern uint8_t USB_TX_Buffer[USB_TX_BUFFER_SIZE];
extern uint32_t USB_TX_Buffer_Pos;

extern USBD_HandleTypeDef hUsbDeviceFS;

#if COMMUNICATION_CHANNEL == COMMUNICATION_USB

void processUSB()
{
	static uint32_t lCNDTR = 0;

	__disable_irq();

	if (lCNDTR != USB_RX_Buffer_Pos)
	{
		static char line[LINE_BUFFER_SIZE];
		static uint32_t linePos = 0;

		int parsedLines = 0;
		while (lCNDTR != USB_RX_Buffer_Pos) {
		  char ch = USB_RX_Buffer[lCNDTR];

		  if (ch == 10) {
			  line[linePos] = 0x00;

			  __enable_irq();
			  parseUsart(line, linePos);
			  __disable_irq();

			  parsedLines++;
			  linePos = 0;
		  } else {
			  line[linePos++] = ch;
		  }

		  if (linePos >= LINE_BUFFER_SIZE) {
			  linePos = 0;
		  }

		  lCNDTR++;
		  lCNDTR %= USB_RX_BUFFER_SIZE;
		}
	}

	USBD_CDC_HandleTypeDef *hcdc =
			(USBD_CDC_HandleTypeDef*) hUsbDeviceFS.pClassData;
	if (USB_TX_Buffer_Pos > 0 && hcdc->TxState == 0) {
		CDC_Transmit_FS(USB_TX_Buffer, USB_TX_Buffer_Pos);
		USB_TX_Buffer_Pos = 0;
	}

	__enable_irq();
}

#endif

#if COMMUNICATION_CHANNEL == COMMUNICATION_UART

void processUart()
{
	static char line[LINE_BUFFER_SIZE];
	static uint32_t linePos = 0;
	static uint32_t lCNDTR = 0;
	if (lCNDTR == 0) {
		lCNDTR = huart1.hdmarx->Instance->CNDTR;
	}

	while (huart1.hdmarx->Instance->CNDTR != lCNDTR) {
	  char ch = DMA_RX_Buffer[DMA_RX_BUFFER_SIZE - lCNDTR];

	  if (ch == 10) {
		  //HAL_UART_Transmit(&huart1, line, linePos, 1000);
		  line[linePos] = 0x00;
		  parseUsart(line, linePos);
		  linePos = 0;
	  } else {
		  line[linePos++] = ch;
	  }

	  if (linePos >= LINE_BUFFER_SIZE) {
		  linePos = 0;
	  }

	  if (lCNDTR > 1) {
		  lCNDTR--;
	  } else {
		  lCNDTR = DMA_RX_BUFFER_SIZE;
	  }
	}
}

#endif

