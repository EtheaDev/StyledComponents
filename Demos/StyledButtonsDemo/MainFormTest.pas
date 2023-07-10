{******************************************************************************}
{                                                                              }
{       StyledButton: a Button Component based on TGraphicControl              }
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
    TopPanel: TPanel;
    StyleLabel: TLabel;
    cbChangeStyle: TComboBox;
    ShowEditButton: TStyledButton;
    VCLButton: TButton;
    LeftPanel: TPanel;
    LeftScrollBox: TScrollBox;
    RightPanel: TPanel;
    RightScrollBox: TScrollBox;
    Panel1: TPanel;
    TopRightPanel: TPanel;
    procedure TestActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbChangeStyleSelect(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ScrollBoxMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    FStyleNames: TStringList;
    procedure BuildStyleList;
    procedure CreateStyledButtons;
    procedure ButtonClick(Sender: TObject);
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
  TStyleManager.TrySetStyle(cbChangeStyle.Text);
end;

procedure TTestMainForm.CreateStyledButtons;
const
  BUTTON_HEIGHT = 28;
  BUTTON_WIDTH = 140;
  BUTTON_MARGIN = 4;
  BUTTON_COL_COUNT = 24;
var
  I, X: integer;
  LStyleName: string;
  LColumn: Integer;

  procedure CreateVCLButton(AColumn, ATop: Integer;
    AStyleName: string);
  begin
    With TButton.Create(Self) do
    begin
      SetBounds((AColumn * BUTTON_WIDTH) + (BUTTON_MARGIN*AColumn),
        ATop,
        BUTTON_WIDTH, BUTTON_HEIGHT);
      StyleName := AStyleName;
      Caption := AStyleName;
      Parent := LeftScrollBox;
      PopupMenu := Self.PopupMenu;
      OnClick := ButtonClick;
    end;
  end;

  procedure CreateStyledButton(AColumn, ATop: Integer;
    AStyleName: string);
  begin
    With TStyledButton.Create(Self) do
    begin
      SetBounds((AColumn * BUTTON_WIDTH) + (BUTTON_MARGIN*AColumn),
        ATop,
        BUTTON_WIDTH, BUTTON_HEIGHT);
      StyleName := AStyleName;
      Caption := AStyleName;
      Parent := RightScrollBox;
      PopupMenu := Self.PopupMenu;
      OnClick := ButtonClick;
    end;
  end;

begin
  X := 0;
  LColumn := 0;
  for I := 0 to FStyleNames.Count -1 do
  begin
    LStyleName := FStyleNames.Strings[I];
    if (I div BUTTON_COL_COUNT) <> LColumn then
    begin
      LColumn := (I div BUTTON_COL_COUNT);
      X := 0;
    end;
    CreateVCLButton(LColumn, X*(BUTTON_HEIGHT+BUTTON_MARGIN), LStyleName);
    CreateStyledButton(LColumn, X*(BUTTON_HEIGHT+BUTTON_MARGIN), LStyleName);
    Inc(X);
  end;
end;

procedure TTestMainForm.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  Caption := Application.Title + ' - ' + Caption;
  FStyleNames := TStringList.Create;
  for I := 0 to High(TStyleManager.StyleNames) do
    FStyleNames.Add(TStyleManager.StyleNames[i]);
  FStyleNames.Sorted := True;

  BuildStyleList;
  CreateStyledButtons;
end;

procedure TTestMainForm.FormDestroy(Sender: TObject);
begin
  FStyleNames.Free;
end;

procedure TTestMainForm.FormResize(Sender: TObject);
begin
  LeftPanel.Width := ClientWidth div 2;
end;

procedure TTestMainForm.ScrollBoxMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
Var
  msg: Cardinal;
  code: Cardinal;
  i, n: Integer;
begin
  Handled := true;
  If ssShift In Shift Then
    msg := WM_HSCROLL
  Else
    msg := WM_VSCROLL;

  If WheelDelta > 0 Then
    code := SB_LINEUP
  Else
    code := SB_LINEDOWN;

  n := Mouse.WheelScrollLines * 4; //Speed Up scrolling
  For i:= 1 to n Do
  begin
    LeftScrollBox.Perform( msg, code, 0 );
    RightScrollBox.Perform( msg, code, 0 );
  end;
  LeftScrollBox.Perform( msg, SB_ENDSCROLL, 0 );
  RightScrollBox.Perform( msg, SB_ENDSCROLL, 0 );
end;

procedure TTestMainForm.TestActionExecute(Sender: TObject);
begin
  EditStyledButton(ShowEditButton);
end;

procedure TTestMainForm.BuildStyleList;
var
  I, SelectedIndex: integer;
  LStyleName, LActiveStyleName: string;
begin
  SelectedIndex := -1;
  cbChangeStyle.Items.Clear;
  LActiveStyleName := TStyleManager.ActiveStyle.Name;
  for i := 0 to FStyleNames.Count -1 do
  begin
    LStyleName := FStyleNames.Strings[I];
    cbChangeStyle.Items.Add(LStyleName);
    if SameText(LStyleName, LActiveStyleName)  then
      SelectedIndex := i;
  end;
  cbChangeStyle.ItemIndex := SelectedIndex;
end;

procedure TTestMainForm.ButtonClick(Sender: TObject);
begin
  if Sender is TButton then
    ShowMessage(TButton(Sender).Caption)
  else if Sender is TStyledButton then
    ShowMessage(TStyledButton(Sender).Caption);
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
end.
