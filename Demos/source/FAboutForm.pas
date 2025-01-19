{******************************************************************************}
{                                                                              }
{  StyledComponents Library                                                    }
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
unit FAboutForm;

interface

uses
  Vcl.StyledButton, Vcl.Imaging.pngimage, Vcl.ExtCtrls, Vcl.Controls,
  System.Classes, Vcl.StdCtrls, Vcl.Forms, Vcl.Graphics,
  Vcl.ButtonStylesAttributes;

type
  TfmAbout = class(TForm)
    OKButton: TStyledButton;
    lbCopyright: TLabel;
    lbAppName: TLabel;
    lbVersion: TLabel;
    paImage: TPanel;
    Image: TImage;
    LinkLabel: TLinkLabel;
    procedure FormCreate(Sender: TObject);
    procedure LinkLabelLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
  public
  end;

procedure ShowAboutForm;

implementation

{$R *.dfm}

uses
  System.SysUtils,
  Winapi.Windows,
  System.Win.ComObj,
  System.UITypes,
  Winapi.ShellAPI;

function GetFileVersion(const FileName: string): string;
var
  FSO: OleVariant;
begin
  FSO := CreateOleObject('Scripting.FileSystemObject');
  Result := FSO.GetFileVersion(FileName);
end;

function GetModuleLocation: string;
begin
  SetLength(Result, 260);
  GetModuleFileName(HInstance, PChar(Result), 260);
  Result:=PChar(Result);
end;

procedure ShowAboutForm;
var
  FAboutForm : TfmAbout;
begin
  FAboutForm := TfmAbout.Create(nil);
  Try
    FAboutForm.ShowModal;
  Finally
    FAboutForm.Free;
  End;  
end;

procedure TfmAbout.FormCreate(Sender: TObject);
begin
  lbAppName.Caption := Application.Title;
  lbVersion.Caption := 'Ver. '+StyledComponentsVersion;
  lbAppName.Font.Style := [TFontStyle.fsBold];
  lbAppName.Font.Style := lbAppName.Font.Style + [TFontStyle.fsBold];
  lbVersion.Font.Style := lbVersion.Font.Style + [TFontStyle.fsBold];
  lbCopyright.Font.Style := lbCopyright.Font.Style + [TFontStyle.fsBold];
end;

procedure TfmAbout.LinkLabelLinkClick(Sender: TObject; const Link: string;
  LinkType: TSysLinkType);
begin
  ShellExecute(handle, 'open',
  PChar(Link), nil, nil, SW_SHOWNORMAL);
//https://github.com/EtheaDev/StyledComponents/wiki
end;

end.

