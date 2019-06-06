unit globals;

{$MODE Delphi}

interface

uses
  Graphics;

const
  ColChanged = clRed;
  ColUnChanged = clGreen;

const
  FLAGS_MAXERROR = 8;
  COMMUNICATION_EOL = chr(10);

  ALT_MODE_POSITION = 0;
  ALT_MODE_VELOCITY = 1;
  ALT_MODE_PWM = 2;

type
  TPIDParams = record
    Valid: boolean;
    Enabled: integer;
    P: double;
    I: double;
    D: double;
    Deadband: double;
    MaxOutput: double;
    FF0: double;
    FF1: double;
    FaultError: double;
    Multiplier: integer;
    TicksPerServo: integer;
  end;

  TStatus = record
    Valid: boolean;
    Enabled: integer;
    Error: double;
    ErrorI: double;
    ErrorD: double;
    Encoder: integer;
    Command: integer;
    Output: double;
    OutputLimited: integer;
    OscVal: integer;
    PidTime: double;
    Flags: word;
    Current: double;
  end;

  TStepInputRespVal = record
    Error: double;
    Output: double;
  end;

function FixDec(s: string): string;
function NormalizeDec(s: string): string;
function GetInt(s: string): integer;
function GetDouble(s: string): double;

implementation

uses SysUtils;

function FixDec(s: string): string;
begin
  if FormatSettings.DecimalSeparator <> '.' then
    Result := StringReplace(s, '.', FormatSettings.DecimalSeparator, [rfReplaceAll])
  else
    Result := s;
end;

function NormalizeDec(s: string): string;
begin
  if FormatSettings.DecimalSeparator <> '.' then
    Result := StringReplace(s, FormatSettings.DecimalSeparator, '.', [rfReplaceAll])
  else
    Result := s;
end;

function GetDouble(s: string): double;
begin
  Result := StrToFloatDef(FixDec(s), -1);
end;

function GetInt(s: string): integer;
begin
  Result := StrToIntDef(FixDec(s), -1);
end;

end.
