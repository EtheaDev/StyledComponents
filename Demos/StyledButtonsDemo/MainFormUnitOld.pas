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
unit MainFormUnitOld;

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
  Vcl.ImgList,
  Vcl.StyledButton,
  Vcl.BootstrapButtonStyles,
  Vcl.StandardButtonStyles,
  System.Actions,
  Vcl.ActnList,
  Vcl.ButtonStylesAttributes,
  Vcl.StyledButtonEditorUnit;

type
  TMainForm = class(TForm)
    NormalButton: TButton;
    StyledButton: TStyledButton;
    ActionList: TActionList;
    TestAction: TAction;
    ImageList: TImageList;
    procedure ButtonClick(Sender: TObject);
    procedure ButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormClick(Sender: TObject);
    procedure TestActionExecute(Sender: TObject);
  private
    procedure CreateAllButtons;
    procedure CreateButtons(const AParent: TWinControl;
      const ALeft, ATop: Integer;
      const AFamily: TStyledButtonFamily;
      const AClass: TStyledButtonClass;
      const ACaption, AVCLStyle: string;
      const AImageIndex: Integer;
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
  EditStyledButton(Sender as TStyledButton);
end;

procedure TMainForm.CreateButtons(const AParent: TWinControl;
  const ALeft, ATop: Integer;
  const AFamily: TStyledButtonFamily;
  const AClass: TStyledButtonClass;
  const ACaption, AVCLStyle: string;
  const AImageIndex: Integer;
  AImagePos: TImageAlignment);
var
  LWidth, LHeight: Integer;
  LStyledButton: TStyledButton;
  LStyledButtonOutline: TStyledButton;
  LStyledButtonDisabled: TStyledButton;
  LStyledButtonVCL: TStyledButton;
begin
  LWidth := 100;
  LHeight := 54;

  LStyledButton := TStyledButton.Create(Self);
  LStyledButton.Parent := AParent;
  LStyledButton.Name := 'StyledButton'+ACaption;
  LStyledButton.Caption := ACaption;
  LStyledButton.Images := ImageList;
  LStyledButton.ImageIndex := AImageIndex;
  LStyledButton.StyleFamily := AFamily;
  LStyledButton.StyleClass := AClass;
  LStyledButton.ImageAlignment := AImagePos;
  LStyledButton.SetBounds(ALeft, ATop, LWidth, LHeight);
  LStyledButton.OnClick := ButtonClick;
  //LStyledButton.OnMouseDown := ButtonMouseDown;

  LStyledButtonOutline := TStyledButton.Create(Self);
  LStyledButtonOutline.Parent := AParent;
  LStyledButtonOutline.Name := 'StyledButtonOutline'+ACaption;
  LStyledButtonOutline.Caption := ACaption;
  LStyledButtonOutline.Images := ImageList;
  LStyledButtonOutline.ImageIndex := AImageIndex;
  LStyledButtonOutline.StyleFamily := AFamily;
  LStyledButtonOutline.StyleClass := AClass;
  LStyledButtonOutline.StyleAppearance := 'outline';
  LStyledButtonOutline.ImageAlignment := AImagePos;
  LStyledButtonOutline.SetBounds(ALeft+150, ATop, LWidth, LHeight);
  //LStyledButtonOutline.OnClick := ButtonClick;
  LStyledButtonOutline.OnMouseDown := ButtonMouseDown;

  LStyledButtonDisabled := TStyledButton.Create(Self);
  LStyledButtonDisabled.Parent := AParent;
  LStyledButtonDisabled.Caption := ACaption;
  LStyledButtonDisabled.Images := ImageList;
  LStyledButtonDisabled.ImageIndex := AImageIndex;
  LStyledButtonDisabled.StyleFamily := AFamily;
  LStyledButtonDisabled.StyleClass := AClass;
  LStyledButtonDisabled.Enabled := False;
  LStyledButtonDisabled.ImageAlignment := AImagePos;
  LStyledButtonDisabled.SetBounds(ALeft+300, ATop, LWidth, LHeight);

  LStyledButtonVCL := TStyledButton.Create(Self);
  LStyledButtonVCL.Parent := AParent;
  LStyledButtonVCL.Caption := AVCLStyle;
  LStyledButtonVCL.Images := ImageList;
  LStyledButtonVCL.ImageIndex := AImageIndex;
  LStyledButtonVCL.StyleFamily := 'Classic';
  LStyledButtonVCL.StyleClass := AVCLStyle;
  LStyledButtonVCL.StyleAppearance := DEFAULT_APPEARANCE;
  LStyledButtonVCL.Enabled := True;
  LStyledButtonVCL.ImageAlignment := AImagePos;
  LStyledButtonVCL.SetBounds(ALeft+450, ATop, LWidth, LHeight);
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
  CreateButtons(Self, 10, 10 , 'Classic', 'Normal','Windows', 'Windows', 0, iaLeft);
  //Bootstrap Buttons
  CreateButtons(Self, 10, 70 , 'Bootstrap', btn_Primary  ,'Primary',  'Tablet Dark',       1, iaRight);
  CreateButtons(Self, 10, 130, 'Bootstrap', btn_Secondary,'Secondary','Lavender Classico', 2, iaTop);
  CreateButtons(Self, 10, 190, 'Bootstrap', btn_Success  ,'Success',  'Iceberg Classico',  3, iaBottom);
  CreateButtons(Self, 10, 250, 'Bootstrap', btn_Danger   ,'Danger',   'Ruby Graphite',     4, iaCenter);
  CreateButtons(Self, 10, 310, 'Bootstrap', btn_Warning  ,'Warning',  'Golden Graphite',  -1, iaLeft);
  CreateButtons(Self, 10, 370, 'Bootstrap', btn_Info     ,'Info',     'Light',            -1, iaRight);
  CreateButtons(Self, 10, 430, 'Bootstrap', btn_Light    ,'Light',    'Luna',             -1, iaTop);
  CreateButtons(Self, 10, 490, 'Bootstrap', btn_Dark     ,'Dark',     'Silver',           -1, iaBottom);
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
