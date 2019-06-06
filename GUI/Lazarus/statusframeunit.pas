unit StatusFrameUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls;

type

  { TStatusFrame }

  TStatusFrame = class(TFrame)
    GroupBoxStatus: TGroupBox;
    Label10: TLabel;
    Label12: TLabel;
    Label14: TLabel;
    Label16: TLabel;
    Label18: TLabel;
    Label20: TLabel;
    Label40: TLabel;
    Label45: TLabel;
    Label49: TLabel;
    Label51: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    lbStatusCurrent: TLabel;
    lbStatusErrorD: TLabel;
    lbStatusErrorI: TLabel;
    lbStatusFlags: TLabel;
    lbStatusOutput: TLabel;
    lbStatusOutputLimited: TLabel;
    lbStatusPidTime: TLabel;
    lbStatusTestOsc: TLabel;
    StatusCommand: TLabel;
    StatusEnabled: TLabel;
    StatusEncoder: TLabel;
    StatusError: TLabel;
    StatusLast: TLabel;
    StatusMaxError: TLabel;
    StatusMaxOutput: TLabel;
  private
  public
    procedure ParseStatus(Parameters: TStrings);
  end;

implementation

{$R *.lfm}

procedure TStatusFrame.ParseStatus(Parameters :TStrings);
begin
 if Parameters.Count = 8 then begin
   StatusError.Caption := Parameters[0];
   StatusCommand.Caption := Parameters[2];
   StatusEncoder.Caption := Parameters[3];
   StatusMaxError.Caption := Parameters[6];
   StatusMaxOutput.Caption := Parameters[7];
    //Status.Command := GetInt(sl[0]);
    //Status.Encoder:= GetInt(sl[1]);
    //Status.Error:= GetDouble(sl[2]);
    //Status.ErrorI:= GetDouble(sl[3]);
    //Status.ErrorD:= GetDouble(sl[4]);
    //Status.Output:= GetDouble(sl[5]);
    //Status.OutputLimited:= GetInt(sl[6]);
    //Status.Enabled:= GetInt(sl[7]);
    //Status.OscVal:= GetInt(sl[8]);
    //Status.PidTime:= GetDouble(sl[9]);
    //Status.Flags:= GetInt(sl[10]);
    //Status.Current:= GetDouble(sl[11]);

    StatusLast.Caption := FormatDateTime('H:m:s', Now());
  end;
end;

end.

