unit main;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, CPort, StdCtrls, ExtCtrls, ComCtrls, globals, Spin, MaskEdit,
  AdvSpinEdit, Menus, ValEdit, ButtonPanel, config, TAGraph, TASeries,
  TASources, SpinEx, DividerBevel, Types, SettingsManager, StatusFrameUnit,
  tuningframeunit, RichMemo;

type
  TLogType = (ltFromDriver, ltToDriver, ltApp);

  { TMainForm }

  TMainForm = class(TForm)
    AltModeSet: TButton;
    AltModeMode: TComboBox;
    StatusFrame_: TStatusFrame;
    WriteSettingsToDriverAuto: TCheckBox;
    GroupBoxSampling1: TGroupBox;
    AltModeParamLabel: TLabel;
    Log: TRichMemo;
    ParametersDeadband: TSpinEditEx;
    ParametersFaultError: TSpinEditEx;
    ParametersKSDI: TAdvSpinEdit;
    ParametersKDI: TAdvSpinEdit;
    ParametersKSDLabel: TLabel;
    ParametersKSD: TAdvSpinEdit;
    ParametersKSDLabel1: TLabel;
    ParametersKDILabel: TLabel;
    ParametersMaxOutput: TSpinEditEx;
    ParametersStepMultiplier: TSpinEditEx;
    ReadSettingsFromDriver: TButton;
    AltModeParam: TSpinEdit;
    WriteSettingsToDriver: TButton;
    btWriteBoardParams: TButton;
    ClearChart: TButton;
    GroupBoxTuning: TGroupBox;
    ParametersPLabel: TLabel;
    ParametersILabel: TLabel;
    ParametersDLabel: TLabel;
    ParametersDeadbandLabel: TLabel;
    ParametersMaxOutputLabel: TLabel;
    ParametersFF0Label: TLabel;
    ParametersFF1Label: TLabel;
    ParametersFaultErrorLabel: TLabel;
    ParametersStepMultiplierLabel: TLabel;
    Label32: TLabel;
    Panel1: TPanel;
    SelectPort: TComboBox;
    Label1: TLabel;
    Connect: TButton;
    Disconnect: TButton;
    ComPort: TComPort;
    PanelChart: TPanel;
    MainTabs: TPageControl;
    ParametersD: TAdvSpinEdit;
    ParametersFF0: TAdvSpinEdit;
    ParametersFF1: TAdvSpinEdit;
    ParametersI: TAdvSpinEdit;
    ParametersP: TAdvSpinEdit;
    seSamples: TSpinEdit;
    ChartLimitSamples: TSpinEdit;
    StaticText1: TStaticText;
    TabManager: TTabSheet;
    PanelLogCommand: TPanel;
    Chart: TChart;
    SeriesError: TLineSeries;
    SeriesOutput: TLineSeries;
    Panel3: TPanel;
    Label2: TLabel;
    Panel4: TPanel;
    Label3: TLabel;
    Panel5: TPanel;
    Label4: TLabel;
    Label5: TLabel;
    PanelTop: TPanel;
    GroupBoxSampling: TGroupBox;
    SamplingStartStop: TButton;
    Label7: TLabel;
    SamplingPerSecond: TSpinEdit;
    btClear: TButton;
    GroupBoxStep: TGroupBox;
    Label11: TLabel;
    cbSIAutoFwdRev: TCheckBox;
    Label13: TLabel;
    seSICommandStep: TSpinEdit;
    seSkipSamples: TSpinEdit;
    LaunchStepResponseTest: TButton;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    SaveParameters1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    ClearLog: TButton;
    TabLog: TTabSheet;
    Panel7: TPanel;
    CommandLine: TEdit;
    Button1: TButton;
    Reset: TButton;
    Error: TPanel;
    SeriesCurrent: TLineSeries;
    procedure AltModeSetClick(Sender: TObject);
    procedure CommandLineChange(Sender: TObject);
    procedure FlowPanel1Click(Sender: TObject);
    procedure LogChange(Sender: TObject);
    procedure ParametersIChange(Sender: TObject);
    procedure SamplingStartStopClick(Sender: TObject);
    procedure ConnectClick(Sender: TObject);
    procedure DisconnectClick(Sender: TObject);
    procedure AltModeModeChange(Sender: TObject);
    procedure ComPortAfterClose(Sender: TObject);
    procedure ComPortRxChar(Sender: TObject; Count: Integer);
    procedure FormShow(Sender: TObject);
    procedure btClearClick(Sender: TObject);
    procedure LaunchStepResponseTestClick(Sender: TObject);
    procedure ReadSettingsFromDriverClick(Sender: TObject);
//    procedure WirteSettingsToDriverClick(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure SaveParameters1Click(Sender: TObject);
    procedure ClearLogClick(Sender: TObject);
    procedure ComPortAfterOpen(Sender: TObject);
    procedure CommandLineKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure MainTabsChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ResetClick(Sender: TObject);
    procedure ClearChartClick(Sender: TObject);
    procedure WriteSettingsToDriverAutoChange(Sender: TObject);
    procedure WriteSettingsToDriverClick(Sender: TObject);
  private
    FWaitRS232: boolean;
    ParamsChanged: TStringList;
    RS232Buf: string;
    LineBuf: TStringList;
    SettingsManager: TSettingsManager;
    procedure ClearRecvBuf();
    procedure initalizeSettingsManager();
    procedure RS232ConnectChanged();
    procedure RS232Write(s :string);
    procedure AddLogLine(s :string; Color: TColor = clBlack);
    procedure AddChartPoint(Error,Output,Current:double);
    function TestStepInputResponse(CommandStep :integer):boolean;
    procedure ParseSample(Parameters :TStrings);
    function ExecuteCommand(Command: String; Parameters: TStrings): Boolean;
    procedure ParseGet(Parameters :TStrings);
    procedure ReadParameters();
    procedure SaveParameters(FileName: string);
    procedure LoadParameters(FileName: string);
    procedure UpdateComPortsList();
    procedure SendParameterToDriver(Key: String; Value: String);
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

procedure TMainForm.UpdateComPortsList();
var
  Ports :TStringList;
  LastPortIndex :Integer;
begin
  EnumComPorts(SelectPort.Items);

  LastPortIndex := SelectPort.Items.IndexOf( Config_ReadString('ComPort') );
  if LastPortIndex > 0 then begin
   SelectPort.ItemIndex := LastPortIndex;
  end;
end;

procedure TMainForm.SendParameterToDriver(Key: String; Value: String);
begin
  RS232Write(Format('SET %s %s', [Key, Value]));
end;

// connect button
procedure TMainForm.ConnectClick(Sender: TObject);
begin
 if (SelectPort.Items.Count = 0) Or (SelectPort.ItemIndex < 0) then begin
  Exit;
 end;

 with ComPort do
  begin
   Port := SelectPort.Items[ SelectPort.ItemIndex ];
   Connected:=false;
   try
     Connected:=true;
   finally
     RS232ConnectChanged();
     FWaitRS232:=false;
   end;
  end;

 Config_WriteString('ComPort', ComPort.Port);
end;

procedure TMainForm.AltModeSetClick(Sender: TObject);
begin
  case AltModeMode.ItemIndex of
       ALT_MODE_POSITION: begin
        RS232Write('MOD 1');
       end;
       ALT_MODE_VELOCITY: begin
        RS232Write(Format('MOD 2 %d', [AltModeParam.Value]));
       end;
       ALT_MODE_PWM: begin
        RS232Write(Format('MOD 3 %d', [AltModeParam.Value]));
       end;
  end;
end;

procedure TMainForm.CommandLineChange(Sender: TObject);
begin

end;

procedure TMainForm.FlowPanel1Click(Sender: TObject);
begin

end;

procedure TMainForm.LogChange(Sender: TObject);
begin

end;

procedure TMainForm.ParametersIChange(Sender: TObject);
begin

end;

procedure TMainForm.SamplingStartStopClick(Sender: TObject);
begin
 RS232Write(Format('SAM %d', [SamplingPerSecond.Value]));
end;

procedure TMainForm.DisconnectClick(Sender: TObject);
begin
 with ComPort do
  begin
   Connected:=false;
   RS232ConnectChanged();
  end;
end;

procedure TMainForm.AltModeModeChange(Sender: TObject);
begin
 case AltModeMode.ItemIndex of
   ALT_MODE_POSITION: begin
    AltModeParam.Enabled := false;
    AltModeParamLabel.Enabled := false;
   end;
   ALT_MODE_VELOCITY: begin
    AltModeParamLabel.Enabled := true;
    AltModeParam.Enabled := true;
    AltModeParam.MaxValue := 50000;
    AltModeParam.Increment := 100;
   end;
   ALT_MODE_PWM: begin
    AltModeParamLabel.Enabled := true;
    AltModeParam.Enabled := true;
    AltModeParam.MaxValue := 100;
    AltModeParam.Increment := 5;
   end;
 end;
end;

procedure TMainForm.ComPortAfterClose(Sender: TObject);
begin
  RS232ConnectChanged();
end;

procedure TMainForm.RS232ConnectChanged();
begin
 with ComPort do
  begin
   RS232Buf := '';
   Connect.Enabled := not Connected;
   SelectPort.Enabled := not Connected;
   Disconnect.Enabled := Connected;
   TabManager.Enabled := Connected;
  end;
end;

procedure TMainForm.RS232Write(s:string);
begin
 ComPort.WriteStr(s + COMMUNICATION_EOL);
 AddLogLine(s, clGreen);
end;

procedure TMainForm.ComPortAfterOpen(Sender: TObject);
begin
  ReadParameters();
end;

procedure TMainForm.ParseSample(Parameters :TStrings);
begin
 if Parameters.Count > 2 then begin
   if Parameters[0] = 'START' then begin
    //SeriesError.Source.BeginUpdate();
    //SeriesOutput.Source.BeginUpdate();
    //SeriesCurrent.Source.BeginUpdate();
   end else
   if Parameters[0] = 'OK' then begin
    //SeriesError.Source.EndUpdate();
    //SeriesOutput.Source.EndUpdate();
    //SeriesCurrent.Source.EndUpdate();
   end else begin
     AddChartPoint(
          StrToFloat(FixDec(Parameters[1])),
          StrToFloat(FixDec(Parameters[2])),
          0 //StrToFloat(FixDec(Elements[3])) / 10
     );
   end;
 end;
end;

function TMainForm.ExecuteCommand(Command: String; Parameters: TStrings): Boolean;
begin
 Result := True;

 if Command = 'GET' then begin

   ParseGet(Parameters);
 end;

 if Command = 'SAM' then begin
   ParseSample(Parameters);
   Result := False;
 end;

 if Command = 'STA' then begin
   StatusFrame_.ParseStatus(Parameters);
   Result := False;
 end;
end;

procedure TMainForm.ParseGet(Parameters: TStrings);
begin
 if Parameters.Count >= 2 then begin
  SettingsManager.ValueUpdated(Parameters[0], Parameters[1]);
 end;
end;

procedure TMainForm.ComPortRxChar(Sender: TObject; Count: Integer);
var
 Buf, Command :string;
 EOLPos :integer;
 Elements :TStringList;
begin
 ComPort.ReadStr(Buf, ComPort.InputCount);
 RS232Buf := RS232Buf + Buf;

 Elements := TStringList.Create();
 Elements.Delimiter := ' ';

 repeat
  EOLPos := Pos(COMMUNICATION_EOL, RS232Buf);
  if EOLPos > 0 then
   begin
    Buf := Copy(RS232Buf, 1, EOLPos - 1);
    RS232Buf := Copy(RS232Buf, EOLPos + Length(COMMUNICATION_EOL), Length(RS232Buf));

    Elements.DelimitedText := Buf;

    Command := Elements[0];
    Elements.Delete(0);

    if ExecuteCommand(Command, Elements) then begin
     AddLogLine(Buf, clBlue);
    end;
   end;
 until (EOLPos=0);

 Elements.Free();
end;

procedure TMainForm.CommandLineKeyPress(Sender: TObject; var Key: Char);
begin
 if Key = #13 then begin
    if ComPort.Connected then begin
     RS232Write(CommandLine.Text);
     CommandLine.Text := '';
    end;
 end;
end;

procedure TMainForm.ClearRecvBuf();
begin
 if ComPort.Connected then ComPort.ClearBuffer(true,true);
 LineBuf.Clear;
end;

procedure TMainForm.AddChartPoint(Error,Output,Current:double);
begin
 SeriesError.Add(Error);
 SeriesOutput.Add(Output);
 SeriesCurrent.Add(Current*20);

 //if (i > Chart.BottomAxis.Range.Max) then
 // begin
 //  Chart.Extent.YMin := Chart.BottomAxis.Range.Min + 1;
 //  Chart.BottomAxis.Range.Max := Chart.BottomAxis.Range.Max + 1;
 // end;

 while SeriesError.Count > ChartLimitSamples.Value do begin
  SeriesError.Delete(0);
  SeriesOutput.Delete(0);
  SeriesCurrent.Delete(0);
 end;
end;

procedure TMainForm.btClearClick(Sender: TObject);
begin
 //SeriesError.Clear();
 //SeriesOutput.Clear();
 //SeriesCurrent.Clear();
end;

function TMainForm.TestStepInputResponse(CommandStep: integer):boolean;
var
 i: integer;
 e,o,c: double;
 sl: TStringList;
begin
 //result:=false;
 //ClearRecvBuf;
// RS232Write(Format('SAM %d %d', [seSkipSamples.Value, seSamples.Value]));
 RS232Write(Format('TES %d %d %d',[CommandStep, seSkipSamples.Value, seSamples.Value]));
// i:=0;
// while (LineBuf.Count<202) and (i<3000) do
//  begin
////  AddLogLine(Format('%d %d',[LineBuf.Count,i]),clRed);
//   Application.ProcessMessages;
//   sleep(1);
//   inc(i);
//  end;
// if (i>=3000) then exit;
////  AddLogLine(Format('%d %d',[LineBuf.Count,i]),clRed);
// i:=0;
// sl := TStringList.Create();
// sl.Delimiter := ' ';
// while (i<202) do
//  begin
//    if Copy(LineBuf.Strings[i],0,5) = 'BEGIN' then begin
//      AddLogLine('Data begin',clBlue);
//      inc(i);
//      continue;
//    end;
//    if Copy(LineBuf.Strings[i],0,5) = 'END' then begin
//      AddLogLine('Dat end',clBlue);
//      inc(i);
//      sl.Free();
//      exit;
//    end;
//    with (sl) do begin
//        DelimitedText := LineBuf.Strings[i];
//        if Count = 3 then begin
//          e:=GetDouble(sl[0]);
//          o:=GetDouble(sl[1]);
//          c:=GetDouble(sl[2]);
//          AddChartPoint(e,o,c);
//        end;
//    end;
//    inc(i);
//  end;
//  sl.Free();
end;

procedure TMainForm.ClearLogClick(Sender: TObject);
begin
 Log.Clear;
end;

procedure TMainForm.LaunchStepResponseTestClick(Sender: TObject);
begin
 btClearClick(Sender);
 //if cbSampleAutomatic.Checked then SampleTimer.Enabled:=false;

 SeriesCurrent.Clear();
 SeriesOutput.Clear();
 SeriesError.Clear();

 Log.Lines.BeginUpdate();
 TestStepInputResponse(seSICommandStep.Value);
 //SampleTimer.Enabled:=true;
 if (cbSIAutoFwdRev.Checked) then seSICommandStep.Value:=-seSICommandStep.Value;
 Log.Lines.Add('---');
 Log.Lines.EndUpdate();
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
 UpdateComPortsList();
end;

procedure TMainForm.MainTabsChange(Sender: TObject);
begin
  case MainTabs.TabIndex of
    0: begin
      MainTabs.Constraints.MinHeight := 450;
      MainTabs.Height := 450;
    end;
    1: begin
      MainTabs.Constraints.MinHeight := 30;
      MainTabs.Height := 30;
    end;
  end;
end;

procedure TMainForm.ReadSettingsFromDriverClick(Sender: TObject);
begin
 ReadParameters();
end;

procedure TMainForm.ResetClick(Sender: TObject);
begin
 if ComPort.Connected then begin
   RS232Write('RES');
//   Error.Hide();
 end;
end;

procedure TMainForm.ClearChartClick(Sender: TObject);
begin
  SeriesCurrent.Clear();
  SeriesError.Clear();
  SeriesOutput.Clear();
end;

procedure TMainForm.WriteSettingsToDriverAutoChange(Sender: TObject);
begin
  SettingsManager.SetAutoUpdate(WriteSettingsToDriverAuto.Checked);
end;

procedure TMainForm.WriteSettingsToDriverClick(Sender: TObject);
begin
  SettingsManager.SendChanged();
end;

procedure TMainForm.ReadParameters();
begin
  RS232Write('GET');
end;

procedure TMainForm.SaveParameters(FileName: string);
begin
 SettingsManager.WriteToFile(FileName);
end;

procedure TMainForm.LoadParameters(FileName: string);
begin
 SettingsManager.ReadFromFile(FileName);
end;

procedure TMainForm.Open1Click(Sender: TObject);
begin
 if (OpenDialog.Execute) then
  begin
   SaveDialog.FileName:=OpenDialog.FileName;
   LoadParameters(OpenDialog.FileName);
  end;
end;

procedure TMainForm.SaveParameters1Click(Sender: TObject);
begin
 if (SaveDialog.Execute) then
  begin
   SaveParameters(SaveDialog.FileName);
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
 ParamsChanged := TStringList.Create();

 SettingsManager := TSettingsManager.Create(SendParameterToDriver);
 InitalizeSettingsManager();
end;

procedure TMainForm.initalizeSettingsManager();
begin
 SettingsManager.Add('P', ParametersP);
 SettingsManager.Add('I', ParametersI);
 SettingsManager.Add('D', ParametersD);
 SettingsManager.Add('MO', ParametersMaxOutput);
 SettingsManager.Add('F0', ParametersFF0);
 SettingsManager.Add('F1', ParametersFF1);
 SettingsManager.Add('FE', ParametersFaultError);
 SettingsManager.Add('SD', ParametersKSD);
 SettingsManager.Add('SDI', ParametersKSDI);
 SettingsManager.Add('DI', ParametersKDI);
 SettingsManager.Add('SM', ParametersStepMultiplier);
 SettingsManager.Add('DB', ParametersDeadband);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
 FormatSettings.DecimalSeparator := ',';
 if (not assigned(LineBuf)) then begin
  LineBuf:=TStringList.Create;
 end;
 UpdateComPortsList();
 RS232ConnectChanged();
 MainTabsChange(nil);
end;

procedure TMainForm.AddLogLine(s: string; Color: TColor = clBlack);
var Pos :Integer;
begin
  with Log do
  begin
   Pos := Length(Lines.Text) - Lines.Count;

   Lines.Add(TimeToStr(Time) + '  ' + trim(s));
   SetRangeColor(Pos, Length(s), Color);

   Lines.BeginUpdate();
   while (Lines.Count > 100) do begin
    Lines.Delete(0);
   end;
   Lines.EndUpdate();

   Perform(EM_SCROLL,SB_PAGEDOWN,0);
   Perform(EM_SCROLL,SB_PAGEDOWN,0);
  end;
end;

end.
