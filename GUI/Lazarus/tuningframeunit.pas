unit tuningframeunit;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, SpinEx, AdvSpinEdit;

type

  { TTuningFrame }

  TTuningFrame = class(TFrame)
    WriteToFlash: TButton;
    GroupBoxTuning: TGroupBox;
    ParametersD: TAdvSpinEdit;
    ParametersDeadband: TSpinEditEx;
    ParametersDeadbandLabel: TLabel;
    ParametersDLabel: TLabel;
    ParametersFaultError: TSpinEditEx;
    ParametersFaultErrorLabel: TLabel;
    ParametersFF0: TAdvSpinEdit;
    ParametersFF0Label: TLabel;
    ParametersFF1: TAdvSpinEdit;
    ParametersFF1Label: TLabel;
    ParametersI: TAdvSpinEdit;
    ParametersILabel: TLabel;
    ParametersKDI: TAdvSpinEdit;
    ParametersKDILabel: TLabel;
    ParametersKSD: TAdvSpinEdit;
    ParametersKSDI: TAdvSpinEdit;
    ParametersKSDLabel: TLabel;
    ParametersKSDLabel1: TLabel;
    ParametersMaxOutput: TSpinEditEx;
    ParametersMaxOutputLabel: TLabel;
    ParametersP: TAdvSpinEdit;
    ParametersPLabel: TLabel;
    ParametersStepMultiplier: TSpinEditEx;
    ParametersStepMultiplierLabel: TLabel;
    ReadSettingsFromDriver: TButton;
    WriteSettingsToDriver: TButton;
    WriteSettingsToDriverAuto: TCheckBox;
  private

  public

  end;

implementation

{$R *.lfm}

end.

