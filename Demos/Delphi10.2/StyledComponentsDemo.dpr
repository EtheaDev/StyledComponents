{******************************************************************************}
{                                                                              }
{  TStyledComponents Demo                                                      }
{  Full demo of Styled Components and Styled TaskDialog/MessageDialog          }
{                                                                              }
{  Copyright (c) 2022-2025 (Ethea S.r.l.)                                      }
{  Author: Carlo Barazzetta                                                    }
{  Contributors:                                                               }
{                                                                              }
{  https://github.com/EtheaDev/StyledComponents                                }
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
program StyledComponentsDemo;

uses
  Midaslib,
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  uSettings in '..\source\uSettings.pas',
  MainDemoForm in '..\source\MainDemoForm.pas' {frmMain},
  DResourcesOld in '..\source\DResourcesOld.pas' {dmResources: TDataModule},
  DemoWelcomeForm in '..\source\DemoWelcomeForm.pas' {WelcomeForm},
  FAboutForm in '..\source\FAboutForm.pas' {fmAbout},
  AutoClickFormOld in '..\source\AutoClickFormOld.pas' {fmAutoClick},
  BitBtnForm in '..\source\BitBtnForm.pas' {fmBitBtn},
  RoundedCornersFormOld in '..\source\RoundedCornersFormOld.pas' {fmRoundedCorners},
  StyledButtonGroupFormOld in '..\source\StyledButtonGroupFormOld.pas' {fmStyledButtonGroup},
  StyledButtonsFormOld in '..\source\StyledButtonsFormOld.pas' {fmStyledButtons},
  StyledCategoryButtonsFormOld in '..\source\StyledCategoryButtonsFormOld.pas' {fmStyledCategoryButtons},
  StyledDbNavigatorFormOld in '..\source\StyledDbNavigatorFormOld.pas' {fmStyledDbNavigator},
  StyledDialogDemoForm in '..\source\StyledDialogDemoForm.pas' {fmStyledTaskDialog},
  StyledToolbarFormOld in '..\source\StyledToolbarFormOld.pas' {fmStyledToolbar};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskBar := True;
  Application.ActionUpdateDelay := 50;
  Application.Title := 'Styled Components Demos with Delphi 10.2';
  //Uses System Style for border / shadow of Forms
  TStyleManager.FormBorderStyle := TStyleManager.TFormBorderStyle.fbsSystemStyle;
  Application.CreateForm(TdmResources, dmResources);
  Application.CreateForm(TfrmMain, frmMain);
  Application.OnException := frmMain.ShowError;
  Application.Run;
end.
