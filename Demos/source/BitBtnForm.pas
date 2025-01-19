{******************************************************************************}
{                                                                              }
{  TStyledBitBtn Demo                                                          }
{  Comparison TStyledBitBtn with TBitBtn                                       }
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
unit BitBtnForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons,
  Vcl.ButtonStylesAttributes, Vcl.StyledButton;

{$INCLUDE StyledComponents.inc}

const
  BTN_ENABLED = True;

type
  TfmBitBtn = class(TForm)
    btOK: TBitBtn;
    btCancel: TBitBtn;
    btClose: TBitBtn;
    btYes: TBitBtn;
    btNo: TBitBtn;
    btAbort: TBitBtn;
    btRetry: TBitBtn;
    btIgnore: TBitBtn;
    btCustom: TBitBtn;
    btHelp: TBitBtn;
    btAll: TBitBtn;
    StyledbtOK: TStyledBitBtn;
    StyledbtCancel: TStyledBitBtn;
    StyledbtClose: TStyledBitBtn;
    StyledbtYes: TStyledBitBtn;
    StyledbtNo: TStyledBitBtn;
    StyledbtAbort: TStyledBitBtn;
    StyledbtRetry: TStyledBitBtn;
    StyledbtIgnore: TStyledBitBtn;
    StyledbtCustom: TStyledBitBtn;
    StyledbtHelp: TStyledBitBtn;
    StyledbtAll: TStyledBitBtn;
    procedure ButtonClick(Sender: TObject);
    procedure ButtonMouseLeave(Sender: TObject);
    procedure ButtonMouseEnter(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function GetScaleFactor: Single;
    procedure CreateButtons(const AStyleFamily: TStyledButtonFamily;
      const ALeftOffSet: Integer);
  public
    { Public declarations }
  end;

var
  fmBitBtn: TfmBitBtn;

implementation

{$R *.dfm}

uses
  Vcl.StandardButtonStyles
  , Vcl.AngularButtonStyles
  , Vcl.BootstrapButtonStyles
  , Vcl.ColorButtonStyles
  , Vcl.StyledTaskDialog
  ;

procedure TfmBitBtn.ButtonClick(Sender: TObject);
var
  LModalResult: Integer;
begin
  if Sender is TBitBtn then
    LModalResult := TBitBtn(Sender).ModalResult
  else if Sender is TStyledBitBtn then
    LModalResult := TStyledBitBtn(Sender).ModalResult
  else
    Exit;
  StyledShowMessage(Format('ModalResult = %d', [LModalResult]));
end;

procedure TfmBitBtn.ButtonMouseEnter(Sender: TObject);
var
  LBtn: TStyledBitBtn;
begin
  if Sender is TStyledBitBtn then
  begin
    LBtn := TStyledBitBtn(Sender);
    Hint := Format('StyleFamily: %s - StyleClass: %s - StyleAppearance: %s',
      [LBtn.StyleFamily, LBtn.StyleClass, LBtn.StyleAppearance]);
  end;
end;

procedure TfmBitBtn.ButtonMouseLeave(Sender: TObject);
begin
  Hint := '';
end;

procedure TfmBitBtn.CreateButtons(const AStyleFamily: TStyledButtonFamily;
  const ALeftOffSet: Integer);

  function CreateButtonAs(const ABitBtn: TBitBtn): TStyledBitBtn;
  begin
    ABitBtn.Enabled := BTN_ENABLED;
    Result := TStyledBitBtn.Create(Self);
    Result.Parent := Self;
    Result.SetBounds(ABitBtn.Left+Round(ALeftOffSet*GetScaleFactor),
      ABitBtn.Top, ABitBtn.Width, ABitBtn.Height);
    Result.StyleFamily := AStyleFamily;
    Result.Caption := ABitBtn.Caption;
    Result.Enabled := BTN_ENABLED;
    Result.Kind := ABitBtn.Kind;
    if Result.Kind = bkCustom then
    begin
      Result.NumGlyphs := ABitBtn.NumGlyphs;
      Result.Glyph := ABitBtn.Glyph;
    end;
    Result.OnClick := ButtonClick;
    Result.OnMouseEnter := ButtonMouseEnter;
    Result.OnMouseLeave := ButtonMouseLeave;
  end;
begin
  CreateButtonAs(btOK);
  CreateButtonAs(btCancel);
  CreateButtonAs(btClose);
  CreateButtonAs(btYes);
  CreateButtonAs(btNo);
  CreateButtonAs(btAbort);
  CreateButtonAs(btRetry);
  CreateButtonAs(btIgnore);
  CreateButtonAs(btHelp);
  CreateButtonAs(btAll);
  CreateButtonAs(btCustom);
end;

procedure TfmBitBtn.FormShow(Sender: TObject);
begin
  //CreateButtons(DEFAULT_CLASSIC_FAMILY, 110); already present at design-time
  CreateButtons(BOOTSTRAP_FAMILY, 220);
  CreateButtons(ANGULAR_LIGHT_FAMILY, 330);
  CreateButtons(ANGULAR_DARK_FAMILY, 440);
  CreateButtons(BASIC_COLOR_FAMILY, 550);
  CreateButtons(SVG_COLOR_FAMILY, 660);
end;

function TfmBitBtn.GetScaleFactor: Single;
begin
  Result := {$IFDEF D10_3+}ScaleFactor{$ELSE}1{$ENDIF};
end;

end.
