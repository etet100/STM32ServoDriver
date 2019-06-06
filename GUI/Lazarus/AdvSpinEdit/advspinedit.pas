unit AdvSpinEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  SpinEx;

type TAdvSpinEditSpinType = (sptFloat);

type

  { TAdvSpinEdit }

  TAdvSpinEdit = class(TFloatSpinEditEx)
  private
    function GetIntValue(): Integer;
    procedure SetIntValue(Val: Integer);

  protected

  public

  published
    property IntValue :Integer read GetIntValue write SetIntValue;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('STM32ServoDriver',[TAdvSpinEdit]);
end;

function FixDec(s :string) :string;
begin
  if FormatSettings.DecimalSeparator <> '.'then
    Result := StringReplace(s, '.', FormatSettings.DecimalSeparator, [rfReplaceAll])
  else
    Result := s;
end;

function TAdvSpinEdit.GetIntValue(): Integer;
begin
  Result := Trunc(Value);
end;

procedure TAdvSpinEdit.SetIntValue(Val :Integer);
begin
  Value := Val;
end;

end.
