(******************************************************
 * ComPort Library ver. 3.0                           *
 *   for Delphi 3, 4, 5, 6, 7 and                     *
 *   C++ Builder 3, 4, 5, 6                           *
 * written by Dejan Crnila, 1998 - 2002               *
 * maintained by Lars B. Dybdahl, 2003                *
 * Homepage: http://comport.sf.net/                   *
 *****************************************************)

unit CPortSetup;

{$mode objfpc}{$H+}



interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, CPort, CPortCtl, LResources;

type
  // TComPort setup dialog

  { TComSetupFrm }

  TComSetupFrm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ComComboBox1: TComComboBox;
    ComComboBox2: TComComboBox;
    ComComboBox3: TComComboBox;
    ComComboBox4: TComComboBox;
    ComComboBox5: TComComboBox;
    ComComboBox6: TComComboBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Combo2: TComComboBox;
    Combo3: TComComboBox;
    Combo4: TComComboBox;
    Combo5: TComComboBox;
    Combo6: TComComboBox;
    Combo1: TComComboBox;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure EditComPort(ComPort: TCustomComPort);

implementation

//uses gnugettext;


// show setup dialog
procedure EditComPort(ComPort: TCustomComPort);
begin
  with TComSetupFrm.Create(nil) do
  begin
    Combo1.ComPort := ComPort;
    Combo2.ComPort := ComPort;
    Combo3.ComPort := ComPort;
    Combo4.ComPort := ComPort;
    Combo5.ComPort := ComPort;
    Combo6.ComPort := ComPort;
    Combo1.UpdateSettings;
    Combo2.UpdateSettings;
    Combo3.UpdateSettings;
    Combo4.UpdateSettings;
    Combo5.UpdateSettings;
    Combo6.UpdateSettings;
    if ShowModal = mrOK then
    begin
      ComPort.BeginUpdate;
      Combo1.ApplySettings;
      Combo2.ApplySettings;
      Combo3.ApplySettings;
      Combo4.ApplySettings;
      Combo5.ApplySettings;
      Combo6.ApplySettings;
      ComPort.EndUpdate;
    end;
    Free;
  end;
end;

procedure TComSetupFrm.FormCreate(Sender: TObject);
begin
  {TP_Ignore(self,'Combo1');
  TP_Ignore(self,'Combo2');
  TP_Ignore(self,'Combo3');
  TP_Ignore(self,'Combo4');
  TP_Ignore(self,'Combo5');
  TP_Ignore(self,'Combo6');
  TranslateProperties (self,'cport');  }
end;

initialization
  {$i CPortSetup.lrs}
  {$I cport.lrs}
end.
