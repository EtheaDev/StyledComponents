{******************************************************************************}
{                                                                              }
{       StyledComponents: a set of Styled VCL Component                        }
{                                                                              }
{       Copyright (c) 2022 (Ethea S.r.l.)                                      }
{       Author: Carlo Barazzetta                                               }
{       Contributors:                                                          }
{                                                                              }
{       https://github.com/EtheaDev/StyledComponents                           }
{                                                                              }
{******************************************************************************}
unit Vcl.StyledComponentsRegister;

interface

uses
  Classes
  , DesignIntf
  , DesignEditors
  , VCLEditors
  , Vcl.StyledButton
  , Vcl.StyledTaskDialog
  ;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Styled Components',
    [TStyledButton,
     TStyledTaskDialog]);
end;

end.
