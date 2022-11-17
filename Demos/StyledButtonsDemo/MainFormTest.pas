{******************************************************************************}
{                                                                              }
{       StyledButton: a Button Component based on TGraphicControl              }
{                                                                              }
{       Copyright (c) 2022 (Ethea S.r.l.)                                      }
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
unit MainFormTest;

interface

{$I ..\..\..\Source\StyledComponents.inc}

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  System.ImageList,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  Vcl.BaseImageCollection,
  Vcl.StyledButton,
  Vcl.BootstrapButtonStyles,
  Vcl.AngularButtonStyles,
  Vcl.StandardButtonStyles,
  System.Actions,
  Vcl.ActnList,
  Vcl.ButtonStylesAttributes,
  Vcl.StyledButtonEditorUnit,
  Vcl.ImageCollection,
  Vcl.Menus,
  Vcl.ComCtrls,
  DResources;

type
  TTestMainForm = class(TForm)
    ActionList: TActionList;
    TestAction: TAction;
    PopupMenu: TPopupMenu;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    Exit1: TMenuItem;
    Panel1: TPanel;
    ShowEditButton: TStyledButton;
    StyleLabel: TLabel;
    cbChangeStyle: TComboBox;
    Button1: TButton;
    procedure TestActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbChangeStyleSelect(Sender: TObject);
  private
    procedure BuildStyleList;
  protected
  end;

var
  TestMainForm: TTestMainForm;

implementation

{$R *.dfm}

uses
  System.TypInfo
  , Vcl.Themes
  , WinApi.ShellAPI
  ;

{ TMainForm }

procedure TTestMainForm.cbChangeStyleSelect(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    TStyleManager.TrySetStyle(cbChangeStyle.Text);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TTestMainForm.FormCreate(Sender: TObject);
begin
  BuildStyleList;
//  ShowEditButton.StyleFamily := 'Angular-light';
//  ShowEditButton.ModalResult := 9;
end;

procedure TTestMainForm.TestActionExecute(Sender: TObject);
begin
  EditStyledButton(ShowEditButton);
end;

procedure TTestMainForm.BuildStyleList;
var
  i, SelectedIndex: integer;
  LStyleName, LActiveStyleName: string;
begin
  SelectedIndex := -1;
  cbChangeStyle.Items.Clear;
  LActiveStyleName := TStyleManager.ActiveStyle.Name;
  for i := 0 to High(TStyleManager.StyleNames) do
  begin
    LStyleName := TStyleManager.StyleNames[i];
    cbChangeStyle.Items.Add(LStyleName);
    if SameText(LStyleName, LActiveStyleName)  then
      SelectedIndex := i;
  end;
  cbChangeStyle.ItemIndex := SelectedIndex;
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
end.
