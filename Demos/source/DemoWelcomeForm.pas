{******************************************************************************}
{                                                                              }
{  Welcome Form for StyledButtonsDemo                                          }
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
unit DemoWelcomeForm;

interface

uses
  Vcl.StdCtrls
  , Vcl.Forms
  , Vcl.Graphics
  , Vcl.ExtCtrls
  , Vcl.Controls
  , System.Classes
  , Vcl.ButtonStylesAttributes
  , Vcl.StyledButton
  , Vcl.Imaging.pngimage
  ;

type
  TWelcomeForm = class(TForm)
    BackPanel: TPanel;
    FramePanel: TPanel;
    TitleLabel: TLabel;
    Memo: TMemo;
    DelphiImage: TImage;
    BottomPanel: TPanel;
    OkButton: TStyledButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
  protected
    procedure Loaded; override;
  end;

implementation

{$R *.dfm}

{ TWelcomeForm }

procedure TWelcomeForm.FormCreate(Sender: TObject);
begin
  Memo.Lines.Clear;
  Memo.Lines.Add('This application shows the use of StyledComponents in various scenario.');
  Memo.Lines.Add('The Main form of the Demo itself is built using some StyledComponents.');
  Memo.Lines.Add('The application can be tested in HighDPI scenario to check ');
  Memo.Lines.Add('if all StyledComponents works fine.');
  Memo.Lines.Add('');
  Memo.Lines.Add('Enjoy with StyledComponents by Ethea!');
end;

procedure TWelcomeForm.FormShow(Sender: TObject);
begin
  if OkButton.CanFocus then
    OkButton.SetFocus;
end;

procedure TWelcomeForm.Loaded;
begin
  TitleLabel.Caption := Application.Title;
  TitleLabel.Font.Color := clWindowText;
  TitleLabel.Font.Style := [fsBold];
  TitleLabel.Font.Height := -20;
  Memo.Font.Height := -14;

  inherited;
end;

procedure TWelcomeForm.OkButtonClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

end.
