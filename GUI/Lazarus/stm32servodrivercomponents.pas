{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit STM32ServoDriverComponents;

{$warn 5023 off : no warning about unused units}
interface

uses
  AdvSpinEdit, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('AdvSpinEdit', @AdvSpinEdit.Register);
end;

initialization
  RegisterPackage('STM32ServoDriverComponents', @Register);
end.
