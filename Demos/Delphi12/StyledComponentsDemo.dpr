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
  DResources in '..\source\DResources.pas' {dmResources: TDataModule},
  DemoWelcomeForm in '..\source\DemoWelcomeForm.pas' {WelcomeForm},
  FAboutForm in '..\source\FAboutForm.pas' {fmAbout},
  AutoClickForm in '..\source\AutoClickForm.pas' {fmAutoClick},
  BitBtnForm in '..\source\BitBtnForm.pas' {fmBitBtn},
  ControlListForm in '..\source\ControlListForm.pas' {fmControlList},
  RoundedCornersForm in '..\source\RoundedCornersForm.pas' {fmRoundedCorners},
  StyledButtonGroupForm in '..\source\StyledButtonGroupForm.pas' {fmStyledButtonGroup},
  StyledButtonsForm in '..\source\StyledButtonsForm.pas' {fmStyledButtons},
  StyledButtonVCLStylesForm in '..\source\StyledButtonVCLStylesForm.pas' {fmStyledButtonVCLStyles},
  StyledCategoryButtonsForm in '..\source\StyledCategoryButtonsForm.pas' {fmStyledCategoryButtons},
  StyledDbNavigatorForm in '..\source\StyledDbNavigatorForm.pas' {fmStyledDbNavigator},
  StyledDialogDemoForm in '..\source\StyledDialogDemoForm.pas' {fmStyledTaskDialog},
  // This form is an example of Custom Form for TaskDialog
  // StyledTaskDialogAnimCustomForm in '..\source\StyledTaskDialogAnimCustomForm.pas' {fmStyledTaskDialogAnimCustom},
  StyledToolbarForm in '..\source\StyledToolbarForm.pas' {fmStyledToolbar};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskBar := True;
  Application.ActionUpdateDelay := 50;
  Application.Title := 'Styled Components Demos with Delphi 12';
  //Uses System Style for border / shadow of Forms
  TStyleManager.FormBorderStyle := TStyleManager.TFormBorderStyle.fbsSystemStyle;
  Application.CreateForm(TdmResources, dmResources);
  Application.CreateForm(TfrmMain, frmMain);
  Application.OnException := frmMain.ShowError;
  Application.Run;
end.
