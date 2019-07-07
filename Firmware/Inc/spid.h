
#ifndef SPID_H
#define SPID_H

#define PWM_MAX 3000.0f

typedef struct {
  //pid
  float kp;
  float ki;
  float kd;
  //differential integrator
  float kdi;
  //scalded differential
  float ksd;
  float fb;
  int32_t fbint;
  //��dana pozycja
  float cmd;
  //limity
  float fault_error;
  float max_error;
  float max_output;
  //float min_output;
  //offset = 0
  float offset;
  //feedforward
  float kff0;
  float kff1;
  float ksdi;
  //
  float error;
  //nasycenie?
  float sat;
  //>0 = enabled
  float en;
  float output;
  //
  int step_multiplier;
  int deadband;
  //
  float error_sum;
  float last_error;
  float last_cmd;
  //
  float stats_max_error;
  float stats_max_output;
} PID;

void resetSPid();
void initSPid();
void calcSPid(float period);
void handleStatus();

#endif
