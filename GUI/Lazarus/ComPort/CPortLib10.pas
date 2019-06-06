{ Ce fichier a été automatiquement créé par Lazarus. Ne pas l'éditer !
This source is only used to compile and install the package.
 }

unit CPortLib10; 

interface

uses
  CPort, CPortSetup, CPortCtl, CPortEsc, CPortTrmSet, CPortReg, 
    LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('CPortReg', @CPortReg.Register); 
end; 

initialization
  RegisterPackage('CPortLib10', @Register); 
end.
