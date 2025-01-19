{******************************************************************************}
{                                                                              }
{  Vcl.StyledGraphicUtils: utilities for Styled Components                     }
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
unit Vcl.StyledGraphicsUtils;

interface

{$INCLUDE StyledComponents.inc}

uses
  Classes
  , ImgList
  , Windows
  , Graphics
  , Vcl.Imaging.pngimage
  , Vcl.StyledButton
  , ComCtrls;

procedure StyledButtonExportToPng(AButtonRender: TStyledButtonRender;
  const AWidth, AHeight: Integer;
  const AOutFolder: string; const AFileName: string = '');

implementation

uses
  System.SysUtils
  , System.Types
  , System.IOUtils
  , Vcl.Themes
  , Vcl.Clipbrd
  ;

// Source: http://www.entwickler-ecke.de/topic_Bitmap+pf32bit+mit+Alpha+afPremultipied+zu+PNG+speichern_103159,0.html
type
  TRGB = packed record B, G, R: byte end;
  TRGBA = packed record B, G, R, A: byte end;
  TRGBAArray = array[0..0] of TRGBA;

{$R-}
function PNG4TransparentBitMap(aBitmap: TBitmap): TPNGImage;
var
  X, Y: integer;
  BmpRGBA: ^TRGBAArray;
  PngRGB: ^TRGB;
begin
  //201011 Thomas Wassermann
  Result := TPNGImage.CreateBlank(COLOR_RGBALPHA, 8, aBitmap.Width , aBitmap.Height);

  Result.CreateAlpha;
  Result.Canvas.CopyMode:= cmSrcCopy;
  Result.Canvas.Draw(0, 0, aBitmap);

  for Y := 0 to Pred(aBitmap.Height) do
  begin
    BmpRGBA := aBitmap.ScanLine[Y];
    PngRGB:= Result.Scanline[Y];
    aBitmap.AlphaFormat := TAlphaFormat.afDefined; // Enable alpha channel
    for X := 0 to Pred(aBitmap.width) do
    begin
      Result.AlphaScanline[Y][X] :=  BmpRGBA[X].A;
      if aBitmap.AlphaFormat in [afDefined, afPremultiplied] then
      begin
        if BmpRGBA[X].A <> 0 then
        begin
          PngRGB^.B := Round(BmpRGBA[X].B / BmpRGBA[X].A * 255);
          PngRGB^.R := Round(BmpRGBA[X].R / BmpRGBA[X].A * 255);
          PngRGB^.G := Round(BmpRGBA[X].G / BmpRGBA[X].A * 255);
        end
        else
        begin
          PngRGB^.B := Round(BmpRGBA[X].B * 255);
          PngRGB^.R := Round(BmpRGBA[X].R * 255);
          PngRGB^.G := Round(BmpRGBA[X].G * 255);
        end;
      end;
      Inc(PngRGB);
    end;
  end;
end;

procedure StyledButtonExportToPng(AButtonRender: TStyledButtonRender;
  const AWidth, AHeight: Integer;
  const AOutFolder: string;
  const AFileName: string = '');
var
  LImagePng: TPngImage;
  LBitmap: TBitmap;
  LFileName: string;
begin
  //Notice: this procedure works fine olny with
  //DrawTextWithGDIPlus and DrawRectWithGDIPlus compiler directives
  LBitmap := nil;
  LImagePng := nil;
  try
    LBitmap := TBitmap.Create;
    LBitmap.PixelFormat := TPixelFormat.pf32bit;   // 32bit bitmap
    LBitmap.AlphaFormat := TAlphaFormat.afDefined; // Enable alpha channel

    // Fill background with transparent
    LBitmap.SetSize(AWidth, AHeight);
    LBitmap.Canvas.Brush.Style := bsSolid;
    LBitmap.Canvas.Brush.Color := AButtonRender.Font.Color;
    LBitmap.Canvas.FillRect(Rect(0, 0, AWidth, AHeight));

    AButtonRender.DrawButton(LBitmap.Canvas, False);

    LImagePng := PNG4TransparentBitMap(LBitmap);
    LFileName := ChangeFileExt(TPath.Combine(AOutFolder,AFileName),'.png');
    LImagePng.SaveToFile(LFileName);
  finally
    LImagePng.free;
    LBitmap.Free;
  end;
end;

end.
