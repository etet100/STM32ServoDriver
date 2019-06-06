unit config;

{$MODE Delphi}

interface

function Config_ReadString(Key :String; Val :String = '') :String;
function Config_ReadInteger(Key :String; Val :Integer = 0) :Integer;
function Config_ReadDouble(Key :String; Val :Double = 0) :Double;

procedure Config_WriteString(Key :String; Val :String);
procedure Config_WriteInteger(Key :String; Val :Integer);
procedure Config_WriteDouble(Key :String; Val :Double);

implementation

uses IniFiles, Forms, SysUtils;

var Ini :TIniFile;

function Config_ReadString(Key :String; Val :String = '') :String;
begin
  Result := Ini.ReadString('STM32Servo', Key, Val)
end;

function Config_ReadInteger(Key :String; Val :Integer) :Integer;
begin

end;

function Config_ReadDouble(Key :String; Val :Double) :Double;
begin

end;

procedure Config_WriteString(Key :String; Val :String);
begin
  Ini.WriteString('STM32Servo', Key, Val)
end;

procedure Config_WriteInteger(Key :String; Val :Integer);
begin

end;

procedure Config_WriteDouble(Key :String; Val :Double);
begin

end;

initialization

Ini := TIniFile.Create(ExtractFilePath(Application.ExeName)+'\STM32ServoDriver.ini');
Ini.WriteString('General', 'Version', '0.1');

finalization

Ini.Free();

end.
