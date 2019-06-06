program STM32ServoDriver;

{$MODE Delphi}

uses
  Forms, Interfaces,
  main in 'main.pas' {fMain},
  globals in 'globals.pas',
  config in 'config.pas';

{$R *.res}

begin
  Application.Initialize();
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
