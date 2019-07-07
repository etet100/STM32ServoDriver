/* USER CODE BEGIN Header */
/**
  ******************************************************************************
  * @file           : main.h
  * @brief          : Header for main.c file.
  *                   This file contains the common defines of the application.
  ******************************************************************************
  * @attention
  *
  * <h2><center>&copy; Copyright (c) 2019 STMicroelectronics.
  * All rights reserved.</center></h2>
  *
  * This software component is licensed by ST under Ultimate Liberty license
  * SLA0044, the "License"; You may not use this file except in compliance with
  * the License. You may obtain a copy of the License at:
  *                             www.st.com/SLA0044
  *
  ******************************************************************************
  */
/* USER CODE END Header */

/* Define to prevent recursive inclusion -------------------------------------*/
#ifndef __MAIN_H
#define __MAIN_H

#ifdef __cplusplus
extern "C" {
#endif

/* Includes ------------------------------------------------------------------*/
#include "stm32f3xx_hal.h"

/* Private includes ----------------------------------------------------------*/
/* USER CODE BEGIN Includes */

/* USER CODE END Includes */

/* Exported types ------------------------------------------------------------*/
/* USER CODE BEGIN ET */

/* USER CODE END ET */

/* Exported constants --------------------------------------------------------*/
/* USER CODE BEGIN EC */

/* USER CODE END EC */

/* Exported macro ------------------------------------------------------------*/
/* USER CODE BEGIN EM */

/* USER CODE END EM */

void HAL_TIM_MspPostInit(TIM_HandleTypeDef *htim);

/* Exported functions prototypes ---------------------------------------------*/
void Error_Handler(void);

/* USER CODE BEGIN EFP */

extern uint8_t servoMode;
extern float servoSpeed;

/* USER CODE END EFP */

/* Private defines -----------------------------------------------------------*/
#define TIM3_NAME "PWM"
#define TIM4_NAME "ENCODER"
#define LED_Pin GPIO_PIN_13
#define LED_GPIO_Port GPIOC
#define ENCODER_I_Pin GPIO_PIN_1
#define ENCODER_I_GPIO_Port GPIOA
#define DEBUG_LED_Pin GPIO_PIN_3
#define DEBUG_LED_GPIO_Port GPIOA
#define ANALOG_OUTPUT_Pin GPIO_PIN_4
#define ANALOG_OUTPUT_GPIO_Port GPIOA
#define PWM_A_Pin GPIO_PIN_6
#define PWM_A_GPIO_Port GPIOA
#define PWM_B_Pin GPIO_PIN_7
#define PWM_B_GPIO_Port GPIOA
#define STEP_Pin GPIO_PIN_0
#define STEP_GPIO_Port GPIOB
#define STEP_EXTI_IRQn EXTI0_IRQn
#define USB_RESET_Pin GPIO_PIN_15
#define USB_RESET_GPIO_Port GPIOB
#define ENABLE_Pin GPIO_PIN_3
#define ENABLE_GPIO_Port GPIOB
#define DIR_Pin GPIO_PIN_4
#define DIR_GPIO_Port GPIOB
#define ENCODER_B_Pin GPIO_PIN_6
#define ENCODER_B_GPIO_Port GPIOB
#define ENCODER_A_Pin GPIO_PIN_7
#define ENCODER_A_GPIO_Port GPIOB
/* USER CODE BEGIN Private defines */

#define USB_RX_BUFFER_SIZE 200

/* USER CODE END Private defines */

#ifdef __cplusplus
}
#endif

#endif /* __MAIN_H */

/************************ (C) COPYRIGHT STMicroelectronics *****END OF FILE****/
