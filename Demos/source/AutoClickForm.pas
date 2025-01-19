{******************************************************************************}
{                                                                              }
{  AutoClick feature of Styledcomponents                                       }
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
unit AutoClickForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ButtonStylesAttributes,
  Vcl.StyledButton, Vcl.StdCtrls, Vcl.Samples.Spin, System.ImageList,
  Vcl.ImgList, Vcl.VirtualImageList;

type
  TfmAutoClick = class(TForm)
    AutoClickButtonRounded: TStyledButton;
    AutoClickButtonRoundRect: TStyledButton;
    AutoClickButtonRect: TStyledButton;
    StartStyledButton: TStyledButton;
    lbAutoClickDelay: TLabel;
    RoundedSpinEdit: TSpinEdit;
    RoundRectSpinEdit: TSpinEdit;
    Label1: TLabel;
    RectSpinEdit: TSpinEdit;
    Label2: TLabel;
    GroupBox1: TGroupBox;
    TestDialogsStyledButton: TStyledButton;
    DialogsSpinEdit: TSpinEdit;
    Label3: TLabel;
    cbAutoClick: TCheckBox;
    cbUseCommandLinks: TCheckBox;
    VirtualImageList32: TVirtualImageList;
    procedure AutoClick(Sender: TObject);
    procedure StartStyledButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TestDialogsStyledButtonClick(Sender: TObject);
  private
    procedure StartAutoClick;
  public
  end;

var
  fmAutoClick: TfmAutoClick;

implementation

{$R *.dfm}

uses
  Vcl.StyledTaskDialog
  ;

procedure TfmAutoClick.AutoClick(Sender: TObject);
var
  LCaption: string;
begin
  LCaption := (Sender as TStyledButton).Caption;
  StyledShowMessageFmt('"%s" - Clicked!', [LCaption]);
end;

procedure TfmAutoClick.FormCreate(Sender: TObject);
begin
  //Initialize Task Dialogs Defaults using Bootstrap buttons:
  InitializeStyledTaskDialogs('Bootstrap');
end;

procedure TfmAutoClick.StartAutoClick;
begin
  AutoClickButtonRounded.AutoClick := False;
  AutoClickButtonRounded.AutoClickDelay := RoundedSpinEdit.Value;
  AutoClickButtonRounded.AutoClick := True;

  AutoClickButtonRoundRect.AutoClick := False;
  AutoClickButtonRoundRect.AutoClickDelay := RoundRectSpinEdit.Value;
  AutoClickButtonRoundRect.AutoClick := True;

  AutoClickButtonRect.AutoClick := False;
  AutoClickButtonRect.AutoClickDelay := RectSpinEdit.Value;
  AutoClickButtonRect.AutoClick := True;
end;

procedure TfmAutoClick.StartStyledButtonClick(Sender: TObject);
begin
  StartAutoClick;
end;

procedure TfmAutoClick.TestDialogsStyledButtonClick(Sender: TObject);
var
  LMessage: string;
var
  LAutoClickDelay: Integer;
begin
  if cbAutoClick.Checked then
    LAutoClickDelay := DialogsSpinEdit.Value
  else
    LAutoClickDelay := -1;

  LMessage :=
    'The file was created: <A HREF="C:\Windows\System32\license.rtf">license.rtf</A>'+sLineBreak+
    'You can run: <A HREF="C:\Windows\System32\Notepad.exe">Notepad Editor</A>'+sLineBreak+
    'You can open folder: <A HREF="C:\Windows\System32\">C:\Windows\System32\</A>'+sLineBreak+
    'You can visit site: <A HREF="http://www.ethea.it">www.Ethea.it</A>';

  if cbUseCommandLinks.Checked then
    DoStyledTaskMessageDlg(
      'Dialog Title - use Command Links', LMessage,
      TMsgDlgType.mtConfirmation,
      [mbAbort, mbRetry, mbIgnore], 0, LAutoClickDelay, True)
  else
    DoStyledTaskMessageDlg('Dialog Title - use normal Buttons', LMessage,
      TMsgDlgType.mtWarning,
      [mbOK, mbCancel], 0, LAutoClickDelay, False)
end;

end.
