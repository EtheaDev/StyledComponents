{******************************************************************************}
{                                                                              }
{       StyledTaskDialogDemo: a Demo for Task Dialog Components                }
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
program StyledTaskDialogDemo;

uses
  Forms,
  MainFormUnit in 'MainFormUnit.pas' {MainForm},
  Vcl.Themes,
  Vcl.Styles,
  Vcl.StyledCmpMessages in '..\..\source\Vcl.StyledCmpMessages.pas',
  Vcl.StyledCmpStrUtils in '..\..\source\Vcl.StyledCmpStrUtils.pas',
  Vcl.StyledTaskDialog in '..\..\source\Vcl.StyledTaskDialog.pas',
  Vcl.StyledTaskDialogFormUnit in '..\..\source\Vcl.StyledTaskDialogFormUnit.pas' {StyledTaskDialogForm},
  Vcl.StyledTaskDialogStdUnit in '..\..\source\Vcl.StyledTaskDialogStdUnit.pas' {StyledTaskDialogStd};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'TaskDialog Demo - Copyright (c) Ethea S.r.l.';
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.OnException := MainForm.showError;
  Application.Run;
end.
