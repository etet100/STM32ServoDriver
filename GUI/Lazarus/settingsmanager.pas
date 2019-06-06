unit SettingsManager;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TSettingType = (stInt, stFloat);
  TSendToDriverEvent = procedure(Key: String; Value: String) of object;

type

  { TSettingsManager }

  TSettingsManager = class
  private
    List: TList;
    SendToDriverEvent: TSendToDriverEvent;
    AutoUpdate_: Boolean;
    procedure CheckParamsChanged();
  public
    constructor Create(SendToDriverEvent :TSendToDriverEvent);
    procedure SendToDriver(Key: String; Value: String);
    procedure Add(Key: string; Component: TComponent);
    procedure ValueUpdated(Key: String; Value: String);
    procedure SendChanged();
    procedure WriteToFile(FileName :String);
    procedure ReadFromFile(FileName :String);
    procedure SetAutoUpdate(Auto :Boolean);
  published
    property AutoUpdate: Boolean write SetAutoUpdate;
  end;

implementation

uses IniFiles, SpinEx, AdvSpinEdit, globals;

type

  { TSetting }

  TSetting = class
  private
    Component: TComponent;
    IntValue :Integer;
    FloatValue: Double;
    Manager :TSettingsManager;
    procedure ParamChanged(Sender: TObject);
    procedure UpdateChangedIndicator();
  public
    Key :String;
    Changed :Boolean;
    constructor Create(Manager :TSettingsManager; Key: string; Component: TComponent);
    procedure ValueUpdated(Value: string);
    procedure SendIfChanged();
  end;

{ TSetting }

procedure TSetting.ParamChanged(Sender: TObject);
begin
  If Manager.AutoUpdate_ then begin
   SendIfChanged();
  end else begin
   UpdateChangedIndicator();
  end;
end;

procedure TSetting.UpdateChangedIndicator();
begin
  if Component is TAdvSpinEdit then begin
   Changed := FloatValue <> TAdvSpinEdit(Component).Value;
   if Changed then begin
    TAdvSpinEdit(Component).Font.Color := ColChanged;
   end else begin
     TAdvSpinEdit(Component).Font.Color := ColUnChanged;
   end;
  end;
  if Component is TSpinEditEx then begin
   Changed := IntValue <> TSpinEditEx(Component).Value;
   if Changed then begin
    TSpinEditEx(Component).Font.Color := ColChanged;
   end else begin
     TSpinEditEx(Component).Font.Color := ColUnchanged;
   end;
  end;
end;

constructor TSetting.Create(Manager :TSettingsManager; Key: string; Component: TComponent);
begin
  self.Component := Component;
  self.Manager := Manager;
  self.Key := Key;
  if Component is TAdvSpinEdit then begin
    TAdvSpinEdit(Component).OnChange := ParamChanged;
  end;
  if Component is TSpinEditEx then begin
    TSpinEditEx(Component).OnChange := ParamChanged;
  end;
end;

procedure TSetting.ValueUpdated(Value: string);
begin
  FloatValue := StrToFloatDef(FixDec(Value), -1);
  if Component is TAdvSpinEdit then begin
    TAdvSpinEdit(Component).Value := FloatValue;
  end;
  if Component is TSpinEditEx then begin
    IntValue := Trunc(FloatValue);
    TSpinEditEx(Component).Value := IntValue;
  end;
  UpdateChangedIndicator();
end;

procedure TSetting.SendIfChanged();
begin
  UpdateChangedIndicator();
  if Changed then begin
   if Component is TAdvSpinEdit then begin
    FloatValue := TAdvSpinEdit(Component).Value;
    Manager.SendToDriver(Key, NormalizeDec(FloatToStr(FloatValue)));
   end;
   if Component is TSpinEditEx then begin
    IntValue := TSpinEditEx(Component).Value;
    Manager.SendToDriver(Key, IntToStr(IntValue));
   end;
   UpdateChangedIndicator();
  end;
end;

{ TSettingsManager }

procedure TSettingsManager.WriteToFile(FileName: String);
var
 IniFile: TIniFile;
begin
 IniFile := TIniFile.Create(FileName);
 with IniFile do
  begin
   //seParametersP.Value:=ReadFloat(Sect,'P',0);
   //seParametersI.Value:=ReadFloat(Sect,'I',0);
   //seParametersD.Value:=ReadFloat(Sect,'D',0);
   //seParametersDeadband.Value:=ReadFloat(Sect,'Deadband',0);
   //seParametersMaxOutput.Value:=ReadInteger(Sect,'MI',95);
   //seParametersFF0.Value:=ReadFloat(Sect,'FF0',0);
   //seParametersFF1.Value:=ReadFloat(Sect,'FF1',0);
   //seParametersFaultError.Value:=ReadInteger(Sect,'FaultError',1000);
   //seParametersStepMultiplier.Value:=ReadInteger(Sect,'StepMultiplier',1);
   //CheckParamsChanged();
  end;
 IniFile.Free();
end;

procedure TSettingsManager.ReadFromFile(FileName: String);
var
 IniFile:TIniFile;
begin
 IniFile:=TIniFile.Create(FileName);
 with IniFile do
  begin
   //WriteFloat(Sect,'P',seParametersP.Value);
   //WriteFloat(Sect,'I',seParametersI.Value);
   //WriteFloat(Sect,'D',seParametersD.Value);
   //WriteFloat(Sect,'Deadband',seParametersDeadband.Value);
   //WriteInteger(Sect,'MO',seParametersMaxOutput.IntValue);
   //WriteFloat(Sect,'FF0',seParametersFF0.Value);
   //WriteFloat(Sect,'FF1',seParametersFF1.Value);
   //WriteInteger(Sect,'FaultError',seParametersFaultError.IntValue);
   //WriteInteger(Sect,'StepMultiplier',seParametersStepMultiplier.IntValue);
  end;
 IniFile.Free;
end;

procedure TSettingsManager.SetAutoUpdate(Auto: Boolean);
begin
  Self.AutoUpdate_ := Auto;
  if Auto then begin
    SendChanged();
  end;
end;

procedure TSettingsManager.CheckParamsChanged();
begin
 //ParamsChanged.Clear();
 //ParamChanged(seParametersP, seParametersP.Value<>ActPIDParams.P);
 //ParamChanged(seParametersI, seParametersI.Value<>ActPIDParams.I);
 //ParamChanged(seParametersD, seParametersD.Value<>ActPIDParams.D);
 //ParamChanged(seParametersDeadband, seParametersDeadband.Value<>ActPIDParams.Deadband);
 //ParamChanged(seParametersMaxOutput, seParametersMaxOutput.Value<>ActPIDParams.MaxOutput);
 //ParamChanged(seParametersFF0, seParametersFF0.Value<>ActPIDParams.FF0);
 //ParamChanged(seParametersFF1, seParametersFF1.Value<>ActPIDParams.FF1);
 //ParamChanged(seParametersFaultError, seParametersFaultError.Value<>round(ActPIDParams.FaultError));
 //ParamChanged(seParametersStepMultiplier, seParametersStepMultiplier.Value<>ActPIDParams.Multiplier);
end;

//// event called by change of any param on UI
//procedure TfMain.seParametersPChange(Sender: TObject);
//begin
// CheckParamsChanged();
//end;
//

constructor TSettingsManager.Create(SendToDriverEvent :TSendToDriverEvent);
begin
 List := TList.Create();
 Self.SendToDriverEvent := SendToDriverEvent;
end;

procedure TSettingsManager.SendToDriver(Key: String; Value: String);
begin
 SendToDriverEvent(Key, Value);
end;

procedure TSettingsManager.Add(Key: string; Component: TComponent);
begin
 List.Add(TSetting.Create(Self, Key, Component));
end;

procedure TSettingsManager.ValueUpdated(Key: String; Value: String);
var I :Integer;
begin
  For I := 0 to List.Count - 1 do begin
    If TSetting(List[I]).Key = Key then begin
       TSetting(List[I]).ValueUpdated(Value);
       Exit;
    end;
  end;
end;

procedure TSettingsManager.SendChanged();
var I :Integer;
begin
  For I := 0 to List.Count - 1 do begin
    TSetting(List[I]).SendIfChanged();
  end;
end;

end.
