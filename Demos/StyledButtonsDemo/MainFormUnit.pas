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
unit MainFormUnit;

interface

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
  SVGIconImageListBase,
  SVGIconImageList,
  Vcl.VirtualImageList,
  Vcl.BaseImageCollection,
  SVGIconImageCollection,
  Vcl.StyledButton,
  Vcl.BootstrapButtonStyles,
  Vcl.StandardButtonStyles,
  System.Actions,
  Vcl.ActnList;

type
  TMainForm = class(TForm)
    VirtualImageList: TVirtualImageList;
    SVGIconImageCollection: TSVGIconImageCollection;
    NormalButton: TButton;
    StyledButton: TStyledButton;
    ActionList: TActionList;
    TestAction: TAction;
    procedure ButtonClick(Sender: TObject);
    procedure ButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormClick(Sender: TObject);
    procedure TestActionExecute(Sender: TObject);
  private
    procedure CreateAllButtons;
    procedure CreateButtons(const AParent: TWinControl; const ALeft,
      ATop: Integer; const AStyle: TStyledButtonStyle;
      const ACaption, AVCLStyle, AImageName: string;
      AImagePos: TImageAlignment);
  protected
    procedure Loaded; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.ButtonMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Sender is TButton then
    ShowMessage(Format('TButton %s Down!',[TButton(Sender).Caption]))
  else if Sender is TStyledButton then
    ShowMessage(Format('TStyledButton %s Down!',[TStyledButton(Sender).Caption]));
end;

procedure TMainForm.TestActionExecute(Sender: TObject);
begin
  ShowMessage('Action executed!');
end;

procedure TMainForm.ButtonClick(Sender: TObject);
begin
  if Sender is TButton then
    ShowMessage(Format('TButton %s clicked!',[TButton(Sender).Caption]))
  else if Sender is TStyledButton then
    ShowMessage(Format('TStyledButton %s clicked!',[TStyledButton(Sender).Caption]));
end;

procedure TMainForm.CreateButtons(const AParent: TWinControl; const ALeft,
  ATop: Integer; const AStyle: TStyledButtonStyle;
  const ACaption, AVCLStyle, AImageName: string;
  AImagePos: TImageAlignment);
var
  LWidth, LHeight: Integer;
  LStyledButton: TStyledButton;
  LStyledButtonOutline: TStyledButton;
  LStyledButtonDisabled: TStyledButton;
  LButton: TButton;
  LButtonDisabled: TButton;
begin
  LWidth := 100;
  LHeight := 54;
  LStyledButton := TStyledButton.Create(Self);
  LStyledButton.Parent := AParent;
  LStyledButton.Name := 'StyledButton'+ACaption;
  LStyledButton.Caption := ACaption;
  LStyledButton.Images := VirtualImageList;
  LStyledButton.ImageName := AImageName;
  LStyledButton.StyleClass := AStyle; //Bootstrap StyleClass
  LStyledButton.ImageAlignment := AImagePos;
  LStyledButton.SetBounds(ALeft, ATop, LWidth, LHeight);
  //LStyledButton.OnClick := ButtonClick;
  LStyledButton.OnMouseDown := ButtonMouseDown;

  LStyledButtonOutline := TStyledButton.Create(Self);
  LStyledButtonOutline.Parent := AParent;
  LStyledButtonOutline.Name := 'StyledButtonOutline'+ACaption;
  LStyledButtonOutline.Caption := ACaption;
  LStyledButtonOutline.Images := VirtualImageList;
  LStyledButtonOutline.ImageName := AImageName;
  LStyledButtonOutline.StyleClass := 'outline-'+AStyle; //Bootstrap outline StyleClass
  LStyledButtonOutline.ImageAlignment := AImagePos;
  LStyledButtonOutline.SetBounds(ALeft+150, ATop, LWidth, LHeight);
  //LStyledButtonOutline.OnClick := ButtonClick;
  LStyledButtonOutline.OnMouseDown := ButtonMouseDown;

  LStyledButtonDisabled := TStyledButton.Create(Self);
  LStyledButtonDisabled.Parent := AParent;
  LStyledButtonDisabled.Caption := ACaption;
  LStyledButtonDisabled.Images := VirtualImageList;
  LStyledButtonDisabled.ImageName := AImageName;
  LStyledButtonDisabled.StyleClass := AStyle;
  LStyledButtonDisabled.Enabled := False;
  LStyledButtonDisabled.ImageAlignment := AImagePos;
  LStyledButtonDisabled.SetBounds(ALeft+300, ATop, LWidth, LHeight);

  LButton := TButton.Create(Self);
  LButton.Parent := AParent;
  LButton.Name := 'Button'+ACaption;
  LButton.Caption := AVCLStyle;
  LButton.Images := VirtualImageList;
  LButton.ImageName := AImageName;
  if AVCLStyle <> '' then
    LButton.StyleName := AVCLStyle;
  LButton.ImageAlignment := AImagePos;
  LButton.SetBounds(ALeft+600, ATop, LWidth, LHeight);
  //LButton.OnClick := ButtonClick;
  LButton.OnMouseDown := ButtonMouseDown;

  LButtonDisabled := TButton.Create(Self);
  LButtonDisabled.Parent := AParent;
  LButtonDisabled.Caption := AVCLStyle;
  LButtonDisabled.Images := VirtualImageList;
  LButtonDisabled.ImageName := AImageName;
  if AVCLStyle <> '' then
    LButtonDisabled.StyleName := AVCLStyle;
  LButtonDisabled.Enabled := False;
  LButtonDisabled.ImageAlignment := AImagePos;
  LButtonDisabled.SetBounds(ALeft+750, ATop, LWidth, LHeight);
end;

procedure TMainForm.FormClick(Sender: TObject);
begin
  if not Assigned(StyledButton.Action) then
  begin
    StyledButton.Action := TestAction;
    NormalButton.Action := TestAction;
  end
  else
  begin
    StyledButton.Action := nil;
    NormalButton.Action := nil;
  end;
end;

procedure TMainForm.CreateAllButtons;
begin
  CreateButtons(Self, 10, 10 , DEFAULT_STYLE_CLASS,'Custom',   'Iceberg Classico',     'About', iaLeft);
  CreateButtons(Self, 10, 70 , btn_Primary,   'Primary',  'Tablet Dark',      'accept_database', iaRight);
  CreateButtons(Self, 10, 130, btn_Secondary, 'Secondary','Windows10',        'add_column', iaTop);
  CreateButtons(Self, 10, 190, btn_Success,   'Success',  'Iceberg Classico', 'address_book', iaBottom);
  CreateButtons(Self, 10, 250, btn_Danger,    'Danger',   'Ruby Graphite',    'alarm_clock', iaCenter);
  CreateButtons(Self, 10, 310, btn_Warning,   'Warning',  'Golden Graphite',  '', iaLeft);
  CreateButtons(Self, 10, 370, btn_Info,      'Info',     'Windows10 Green',  '', iaRight);
  CreateButtons(Self, 10, 430, btn_Light,     'Light',    'Tablet Light',     '', iaTop);
  CreateButtons(Self, 10, 490, btn_Dark,      'Dark',     'Windows10 Dark',   '', iaBottom);
end;

procedure TMainForm.Loaded;
begin
  CreateAllButtons;
  inherited;
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
end.
