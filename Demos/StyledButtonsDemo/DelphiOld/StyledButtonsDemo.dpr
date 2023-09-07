{******************************************************************************}
{                                                                              }
{       StyledButtonsDemo: a Demo to show StyledButtons                        }
{       with different Familes (Classic, Bootstrap and Angular                 }
{                                                                              }
{       Copyright (c) 2022-2023 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{       Contributors:                                                          }
{                                                                              }
{       https://github.com/EtheaDev/StyledComponents                           }
{                                                                              }
{******************************************************************************}
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License");             }
{  you may not use this file except in compliance with the License.            }
{  You may obtain a copy of the License at                                     }
{                                                                              }
{      http://www.apache.org/licenses/LICENSE-2.0                              }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS,           }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{******************************************************************************}
program StyledButtonsDemo;

uses
  Vcl.Forms,
  MainFormUnitOld in '..\MainFormUnitOld.pas' {MainForm},
  Vcl.Themes,
  Vcl.Styles,
  Vcl.StyledButton in '..\..\..\source\Vcl.StyledButton.pas',
  Vcl.BootstrapButtonStyles in '..\..\..\source\Vcl.BootstrapButtonStyles.pas',
  Vcl.AngularButtonStyles in '..\..\..\source\Vcl.AngularButtonStyles.pas',
  Vcl.StandardButtonStyles in '..\..\..\source\Vcl.StandardButtonStyles.pas',
  Vcl.StyledButtonEditorUnit in '..\..\..\packages\Vcl.StyledButtonEditorUnit.pas' {StyledButtonEditor};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Styled Buttons Demo - (c) Copyright Ethea S.r.l.';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
