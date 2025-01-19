{******************************************************************************}
{                                                                              }
{  StyledCategoryButtons: a Styled CategoryButtons with TStyledButtonItem      }
{  Based on TCategoryButtons and TButtonItem                                   }
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
unit Vcl.StyledCategoryButtons;

interface

{$INCLUDE StyledComponents.inc}

uses
  Vcl.ImgList
  , System.UITypes
  , System.SysUtils
  , System.Classes
  , System.Math
  , Vcl.ToolWin
  , Vcl.ComCtrls
  , Vcl.StdCtrls
  , Vcl.ExtCtrls
  , Vcl.Themes
  , Vcl.Controls
  , Vcl.ActnList
  , Vcl.Menus
  , Vcl.CategoryButtons
  , Winapi.Messages
  , Winapi.Windows
  , Vcl.StyledButton
  , Vcl.ButtonStylesAttributes
  , Vcl.StandardButtonStyles
  , Vcl.Graphics
  ;

resourcestring
  ERROR_SETTING_CATEGORYBUTTONS_STYLE = 'Error setting CategoryButtons Style: %s/%s/%s not available';

type
  EStyledCategoryButtonsError = Exception;

  TStyledCategoryButtons = class;
  TStyledCategoryButtonsClass = class of TStyledCategoryButtons;
  TStyledButtonItem = class;
  TStyledButtonItemClass = class of TButtonItem;
  TStyledButtonCategory = class;
  TStyledButtonCategoryClass = class of TStyledButtonCategory;
  TStyledButtonCategories = class;
  TStyledButtonCategoriesClass = class of TStyledButtonCategories;


  TButtonProc = reference to procedure (Button: TStyledButtonItem);

  TGetCategoryButtonsBadgeInfo = procedure (
    const ACategoryIndex: Integer;
    const AButtonItemIndex: Integer;
    var ABadgeContent: string;
    var ASize: TNotificationBadgeSize;
    var APosition: TNotificationBadgePosition;
    var AColor: TColor;
    var AFontColor: TColor;
    var AFontStyle: TFontStyles) of Object;

  { TStyledButtonCategory }
  TStyledButtonCategory = class(TButtonCategory)
  protected
  public
    constructor Create(Collection: TCollection); override;
  end;

  { TStyledButtonCategories }
  TStyledButtonCategories = class(TButtonCategories)
  private
    function GetStyledCategoryButtons: TStyledCategoryButtons;
    function GetItem(Index: Integer): TStyledButtonCategory;
    procedure SetItem(Index: Integer; const AValue: TStyledButtonCategory);
  public
    constructor Create(const CategoryButtons: TCategoryButtons); override;
    function Add: TStyledButtonCategory;
    property CategoryButtons: TStyledCategoryButtons read GetStyledCategoryButtons;
    property Items[Index: Integer]: TStyledButtonCategory read GetItem write SetItem; default;
  end;

  { TStyledButtonItem }
  TStyledButtonItem = class(TButtonItem)
  private
    FCollection: TButtonCollection;
    //Styled Attributes
    FStyleRadius: Integer;
    FStyleRoundedCorners: TRoundedCorners;
    FStyleDrawType: TStyledButtonDrawType;
    FStyleFamily: TStyledButtonFamily;
    FStyleClass: TStyledButtonClass;
    FStyleAppearance: TStyledButtonAppearance;
    FStyleApplied: Boolean;
    procedure InvalidateOwner;
    function IsCustomDrawType: Boolean;
    function IsCustomRoundedCorners: Boolean;
    function IsCustomRadius: Boolean;
    function IsStoredStyle: Boolean;
    procedure SetStyleFamily(const AValue: TStyledButtonFamily);
    procedure SetStyleClass(const AValue: TStyledButtonClass);
    procedure SetStyleAppearance(const AValue: TStyledButtonAppearance);
    procedure SetStyleDrawType(const AValue: TStyledButtonDrawType);
    procedure SetStyleRadius(const AValue: Integer);
    procedure SetStyleRoundedCorners(const AValue: TRoundedCorners);
    function GetStyledCategoryButtons: TStyledCategoryButtons;
    function ApplyButtonStyle: Boolean;
    procedure LoadDefaultStyles;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    procedure SetButtonStyle(const AStyleFamily: TStyledButtonFamily;
      const AStyleClass: TStyledButtonClass;
      const AStyleAppearance: TStyledButtonAppearance);
  published
    property CategoryButtons: TStyledCategoryButtons read GetStyledCategoryButtons;

    //StyledComponents Attributes
    property StyleRadius: Integer read FStyleRadius write SetStyleRadius stored IsCustomRadius;
    property StyleDrawType: TStyledButtonDrawType read FStyleDrawType write SetStyleDrawType stored IsCustomDrawType;
    property StyleRoundedCorners: TRoundedCorners read FStyleRoundedCorners write SetStyleRoundedCorners stored IsCustomRoundedCorners;
    property StyleFamily: TStyledButtonFamily read FStyleFamily write SetStyleFamily stored IsStoredStyle;
    property StyleClass: TStyledButtonClass read FStyleClass write SetStyleClass stored IsStoredStyle;
    property StyleAppearance: TStyledButtonAppearance read FStyleAppearance write SetStyleAppearance stored IsStoredStyle;
  end;


  { TStyledCategoryButtons }
  [ComponentPlatforms(pidWin32 or pidWin64)]
  TStyledCategoryButtons = class(TCategoryButtons)
  private
    //StyledButton Attributes
    FButtonStyleNormal: TStyledButtonAttributes;
    FButtonStylePressed: TStyledButtonAttributes;
    FButtonStyleSelected: TStyledButtonAttributes;
    FButtonStyleHot: TStyledButtonAttributes;
    FButtonStyleDisabled: TStyledButtonAttributes;

    //Styled Attributes
    FStyleRadius: Integer;
    FStyleDrawType: TStyledButtonDrawType;
    FStyleRoundedCorners: TRoundedCorners;
    FStyleFamily: TStyledButtonFamily;
    FStyleClass: TStyledButtonClass;
    FStyleAppearance: TStyledButtonAppearance;
    FCustomDrawType: Boolean;
    FStyleApplied: Boolean;

    //Notification Badge event handler
    FOnGetNotificationBadgeInfo: TGetCategoryButtonsBadgeInfo;

    FCaptionAlignment: TAlignment;
    FImageAlignment: TImageAlignment;
    FImageMargins: TImageMargins;
    FSpacing: Integer;
    FFlat: Boolean;
    FButtonsCursor: TCursor;
    FCursor: TCursor;

    class var
    _DefaultStyleDrawType: TStyledButtonDrawType;
    _UseCustomDrawType: Boolean;
    _DefaultFamily: TStyledButtonFamily;
    _DefaultClass: TStyledButtonClass;
    _DefaultAppearance: TStyledButtonAppearance;
    _DefaultStyleRadius: Integer;
    _DefaultButtonsCursor: TCursor;

    procedure ImageMarginsChange(Sender: TObject);
    function IsCustomDrawType: Boolean;
    function IsCustomRoundedCorners: Boolean;
    function IsCustomRadius: Boolean;
    function ImageMarginsStored: Boolean;
    function IsStoredStyleAppearance: Boolean;
    function IsStoredStyleClass: Boolean;
    function IsStoredStyleFamily: Boolean;
    procedure SetStyleAppearance(const AValue: TStyledButtonAppearance);
    procedure SetStyleClass(const AValue: TStyledButtonClass);
    procedure SetStyleDrawType(const AValue: TStyledButtonDrawType);
    procedure SetStyleFamily(const AValue: TStyledButtonFamily);
    procedure SetStyleRadius(const AValue: Integer);
    procedure SetStyleRoundedCorners(const AValue: TRoundedCorners);
    function ApplyCategoryButtonsStyle: Boolean;
    procedure SetStyleApplied(const AValue: Boolean);
    function ApplyButtonStyle: Boolean;
    function GetActiveStyleName: string;
    function AsVCLStyle: Boolean;
    function GetAsVCLComponent: Boolean;
    procedure SetAsVCLComponent(const AValue: Boolean);
    procedure ProcessButtons(AButtonProc: TButtonProc);
    procedure SetButtonStyleDisabled(const AValue: TStyledButtonAttributes);
    procedure SetButtonStyleHot(const AValue: TStyledButtonAttributes);
    procedure SetButtonStyleNormal(const AValue: TStyledButtonAttributes);
    procedure SetButtonStylePressed(const AValue: TStyledButtonAttributes);
    procedure SetButtonStyleSelected(const AValue: TStyledButtonAttributes);
    procedure GetDrawingStyle(const ACanvas: TCanvas;
      const AButtonState: TStyledButtonState;
      const AItem: TStyledButtonItem);
    function GetAttributes(const AMode: TStyledButtonState): TStyledButtonAttributes;
    procedure DrawBackgroundAndBorder(const ACanvas: TCanvas;
      const ADrawRect, ADropDownRect: TRect;
      const AStyleDrawType: TStyledButtonDrawType;
      const ARadius: Single; const ARoundedCorners: TRoundedCorners);
    procedure DrawCaptionAndImage(const ACanvas: TCanvas; const ASurfaceRect: TRect;
      const ACaption: TCaption; const AImageIndex: Integer);
    procedure SetCaptionAlignment(const AValue: TAlignment);
    function GetImageSize(out AWidth, AHeight: Integer;
      out AImageList: TCustomImageList): boolean;
    procedure DrawText(const ASurfaceRect: TRect;
      const ACanvas: TCanvas; const AText: string;
      const AAlignment: TAlignment; const ASpacing: Integer; var ARect: TRect;
      AFlags: Cardinal);
    procedure SetSpacing(const AValue: Integer);
    procedure SetFlat(const AValue: Boolean);
    function IsStyleEnabled: Boolean;
    procedure SetCursor(const AValue: TCursor);
    procedure SetImageAlignment(const AValue: TImageAlignment);
    function GetScaleFactor: Single;
    function CalcMaxBorderWidth: Integer;
    function GetButtonCategories: TStyledButtonCategories;
    procedure SetButtonCategories(const AValue: TStyledButtonCategories);
    procedure SetImageMargins(const AValue: TImageMargins);
    function IsDefaultImageMargins: Boolean;
    procedure CalcDefaultImageMargins(const AValue: TImageAlignment);
    {$IFNDEF D10_4+}
    function IsCustomStyleActive: Boolean;
    {$ENDIF}
    //Windows messages
    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;
  protected
    {$IFDEF D10_1+}
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    {$ENDIF}
    procedure Loaded; override;

    function GetButtonCategoriesClass: TButtonCategoriesClass; override;
    function GetButtonCategoryClass: TButtonCategoryClass; override;
    function GetButtonItemClass: TButtonItemClass; override;

    procedure UpdateStyleElements; override;
    procedure DrawButton(const AButton: TButtonItem; ACanvas: TCanvas;
      ARect: TRect; AState: TButtonDrawState); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
  public
    procedure Assign(Source: TPersistent); override;
    function StyledButtonState(const AState: TButtonDrawState): TStyledButtonState;
    class procedure RegisterDefaultRenderingStyle(
      const ADrawType: TStyledButtonDrawType;
      const AFamily: TStyledButtonFamily = DEFAULT_CLASSIC_FAMILY;
      const AClass: TStyledButtonClass = DEFAULT_WINDOWS_CLASS;
      const AAppearance: TStyledButtonAppearance = DEFAULT_APPEARANCE;
      const AStyleRadius: Integer = DEFAULT_RADIUS;
      const AButtonsCursor: TCursor = DEFAULT_CURSOR); virtual;
    procedure SetCategoryButtonsStyle(const AStyleFamily: TStyledButtonFamily;
      const AStyleClass: TStyledButtonClass;
      const AStyleAppearance: TStyledButtonAppearance);
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    //Styled constructor
    constructor CreateStyled(AOwner: TComponent;
      const AFamily: TStyledButtonFamily;
      const AClass: TStyledButtonClass;
      const AAppearance: TStyledButtonAppearance); virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property AsVCLComponent: Boolean read GetAsVCLComponent write SetAsVCLComponent stored False;
    property StyleApplied: Boolean read FStyleApplied write SetStyleApplied;
  published
    property ButtonsCursor: TCursor read FButtonsCursor write FButtonsCursor default DEFAULT_CURSOR;
    property Cursor: TCursor read FCursor write SetCursor default crDefault;
    property CaptionAlignment: TAlignment read FCaptionAlignment write SetCaptionAlignment default taLeftJustify;
    property Categories: TStyledButtonCategories read GetButtonCategories write SetButtonCategories;
    property ImageAlignment: TImageAlignment read FImageAlignment write SetImageAlignment default iaLeft;
    property ImageMargins: TImageMargins read FImageMargins write SetImageMargins stored ImageMarginsStored;
    property Flat: Boolean read FFlat write SetFlat default False;
    property Spacing: Integer read FSpacing write SetSpacing default 4;

    //StyledButton Attributes
    property ButtonStyleNormal: TStyledButtonAttributes read FButtonStyleNormal write SetButtonStyleNormal;
    property ButtonStylePressed: TStyledButtonAttributes read FButtonStylePressed write SetButtonStylePressed;
    property ButtonStyleSelected: TStyledButtonAttributes read FButtonStyleSelected write SetButtonStyleSelected;
    property ButtonStyleHot: TStyledButtonAttributes read FButtonStyleHot write SetButtonStyleHot;
    property ButtonStyleDisabled: TStyledButtonAttributes read FButtonStyleDisabled write SetButtonStyleDisabled;

    //StyledComponents Attributes
    property StyleRadius: Integer read FStyleRadius write SetStyleRadius stored IsCustomRadius;
    property StyleDrawType: TStyledButtonDrawType read FStyleDrawType write SetStyleDrawType stored IsCustomDrawType;
    property StyleRoundedCorners: TRoundedCorners read FStyleRoundedCorners write SetStyleRoundedCorners stored IsCustomRoundedCorners;
    property StyleFamily: TStyledButtonFamily read FStyleFamily write SetStyleFamily stored IsStoredStyleFamily;
    property StyleClass: TStyledButtonClass read FStyleClass write SetStyleClass stored IsStoredStyleClass;
    property StyleAppearance: TStyledButtonAppearance read FStyleAppearance write SetStyleAppearance stored IsStoredStyleAppearance;

    //Notification Badge Info Event Handler
    property OnGetNotificationBadgeInfo: TGetCategoryButtonsBadgeInfo read FOnGetNotificationBadgeInfo write FOnGetNotificationBadgeInfo;
  end;

implementation

uses
  Vcl.Consts
  , Vcl.Forms
  , System.Types
  , System.RTLConsts
  ;

const
  DEFAULT_IMAGE_HMARGIN = 2;
  DEFAULT_IMAGE_VMARGIN = 2;

{ TStyledCategoryButtons }

constructor TStyledCategoryButtons.CreateStyled(AOwner: TComponent;
  const AFamily: TStyledButtonFamily; const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance);
begin
  Assert(Assigned(AOwner));
  inherited Create(AOwner);

  //new properties for StyledCategoryButtons
  FCaptionAlignment := taLeftJustify;
  FSpacing := 4;
  FFlat := False;
  FImageAlignment := iaLeft;
  FButtonsCursor := _DefaultButtonsCursor;
  FImageMargins := TImageMargins.Create;
  FImageMargins.Left := 2;
  FImageMargins.OnChange := ImageMarginsChange;

  FButtonStyleNormal := TStyledButtonAttributes.Create(Self);
  FButtonStyleNormal.Name := 'Normal';
  FButtonStylePressed := TStyledButtonAttributes.Create(Self);
  FButtonStylePressed.Name := 'Pressed';
  FButtonStyleSelected := TStyledButtonAttributes.Create(Self);
  FButtonStyleSelected.Name := 'Selected';
  FButtonStyleHot := TStyledButtonAttributes.Create(Self);
  FButtonStyleHot.Name := 'Hot';
  FButtonStyleDisabled := TStyledButtonAttributes.Create(Self);
  FButtonStyleDisabled.Name := 'Disabled';

  FStyleDrawType := _DefaultStyleDrawType;
  FStyleRadius := _DefaultStyleRadius;
  FStyleRoundedCorners := ALL_ROUNDED_CORNERS;
  FStyleFamily := AFamily;
  FStyleClass := AClass;
  FStyleAppearance := AAppearance;
end;

procedure TStyledCategoryButtons.CMStyleChanged(var Message: TMessage);
begin
  inherited;
  ApplyButtonStyle;
end;

{$IFDEF D10_1+}
procedure TStyledCategoryButtons.ChangeScale(M, D: Integer;
  isDpiChange: Boolean);
begin
  inherited;
  {$IFNDEF D10_4+}
  //Fixed in Delphi 10.4
  ButtonWidth := MulDiv(ButtonWidth, M, D);
  ButtonHeight := MulDiv(ButtonHeight, M, D);
  Resize;
  UpdateAllButtons;
  {$ENDIF}
end;
{$ENDIF}

constructor TStyledCategoryButtons.Create(AOwner: TComponent);
begin
  CreateStyled(AOwner,
    _DefaultFamily,
    _DefaultClass,
    _DefaultAppearance);
end;

destructor TStyledCategoryButtons.Destroy;
begin
  FreeAndNil(FImageMargins);
  FreeAndNil(FButtonStyleNormal);
  FreeAndNil(FButtonStylePressed);
  FreeAndNil(FButtonStyleSelected);
  FreeAndNil(FButtonStyleHot);
  FreeAndNil(FButtonStyleDisabled);
  inherited Destroy;
end;

function TStyledCategoryButtons.GetScaleFactor: Single;
begin
  Result := {$IFDEF D10_3+}ScaleFactor{$ELSE}1{$ENDIF};
end;

procedure TStyledCategoryButtons.DrawBackgroundAndBorder(
  const ACanvas: TCanvas; const ADrawRect, ADropDownRect: TRect;
  const AStyleDrawType: TStyledButtonDrawType;
  const ARadius: Single; const ARoundedCorners: TRoundedCorners);
var
  LButtonOffset: Integer;
  LDropDownRect: TRect;
  LScaleFactor: Single;
begin
  LScaleFactor := GetScaleFactor;
  LDropDownRect := ADropDownRect;
  //Draw Button Shape
  CanvasDrawshape(ACanvas, ADrawRect, AStyleDrawType,
    ARadius*LScaleFactor, ARoundedCorners);

  //Draw Bar and Triangle
  if LDropDownRect.Width > 0 then
  begin
    if not (AStyleDrawType in [btRounded, btEllipse]) then
    begin
      CanvasDrawBar(ACanvas, LDropDownRect,
        LScaleFactor,
        ACanvas.Pen.Color);
      CanvasDrawTriangle(ACanvas, LDropDownRect,
        LScaleFactor,
        ACanvas.Font.Color);
    end
    else
    begin
      LButtonOffset := LDropDownRect.Height div 8;
      LDropDownRect.Left := LDropDownRect.Left - LButtonOffset;
      LDropDownRect.Right := LDropDownRect.Right - LButtonOffset;
      CanvasDrawTriangle(ACanvas, LDropDownRect,
        LScaleFactor,
        ACanvas.Font.Color);
    end;
  end;
end;

function TStyledCategoryButtons.GetImageSize(out AWidth, AHeight: Integer;
  out AImageList: TCustomImageList): boolean;
begin
  AWidth := 0;
  AHeight := 0;
  //Return True if using ImageList
  if Assigned(Images) then
  begin
    AWidth := Images.Width;
    AHeight := Images.Height;
    Result := True;
  end
  else
    Result := False;
end;

procedure TStyledCategoryButtons.DrawText(
  const ASurfaceRect: TRect;
  const ACanvas: TCanvas;
  const AText: string; const AAlignment: TAlignment;
  const ASpacing: Integer;
  var ARect: TRect; AFlags: Cardinal);
var
  R: TRect;
  LText: string;
begin
  //Drawing Caption
  R := ARect;
  LText := AText;
  Winapi.Windows.DrawText(ACanvas.Handle, PChar(AText), Length(AText),
    R, AFlags or DT_CALCRECT);
  case AAlignment of
    taLeftJustify:
    begin
      OffsetRect(R, ASpacing, (ARect.Height - R.Height) div 2);
    end;
    taRightJustify:
    begin
      OffsetRect(R, ARect.Width - R.Width - ASpacing, (ARect.Height - R.Height) div 2);
    end;
    else
    begin
      OffsetRect(R, (ARect.Width - R.Width) div 2, (ARect.Height - R.Height) div 2);
    end;
  end;
  if ASurfaceRect.Right < R.Right + ASpacing then
    R.Right := ASurfaceRect.Right - ASpacing;
  if ASurfaceRect.Left > R.Left - ASpacing then
    R.Left := ASurfaceRect.Left + ASpacing;
  ACanvas.TextRect(R, LText, [tfEndEllipsis]);
end;

procedure TStyledCategoryButtons.DrawCaptionAndImage(
  const ACanvas: TCanvas; const ASurfaceRect: TRect;
  const ACaption: TCaption; const AImageIndex: Integer);
var
  LTextFlags: Cardinal;
  LImageRect, LTextRect: TRect;
  LImageList: TCustomImageList;
  LImageWidth, LImageHeight: Integer;
  LUseImageList: Boolean;
begin
  if boShowCaptions in ButtonOptions then
  begin
    case FCaptionAlignment of
      taLeftJustify: LTextFlags := DT_NOCLIP or DT_LEFT or DT_VCENTER;
      taRightJustify: LTextFlags := DT_NOCLIP or DT_RIGHT or DT_VCENTER;
    else
      LTextFlags := DT_NOCLIP or DT_CENTER or DT_VCENTER;
    end;
    LTextFlags := DrawTextBiDiModeFlags(LTextFlags);
    (*
    if FWordWrap then
      LTextFlags := LTextFlags or DT_WORDBREAK;
    *)
  end
  else
  begin
    LTextFlags := DT_NOCLIP or DT_CENTER or DT_VCENTER;
  end;
  LUseImageList := GetImageSize(LImageWidth, LImageHeight, LImageList);

  //Calculate LTextRect and LImageRect using ImageMargins and ImageAlignment
  CalcImageAndTextRect(ASurfaceRect, ACaption, LTextRect, LImageRect,
    LImageWidth, LImageHeight, FImageAlignment, FImageMargins,
    CalcMaxBorderWidth, GetScaleFactor);

  if LUseImageList and not Assigned(OnDrawIcon) then
  begin
    //Uses an ImageList to draw the Icon
    Images.Draw(ACanvas, LImageRect.Left, LImageRect.Top,
      AImageIndex, Enabled);
  end;

  if boShowCaptions in ButtonOptions then
    DrawText(ASurfaceRect, ACanvas, ACaption, FCaptionAlignment, FSpacing,
      LTextRect, LTextFlags);
end;

{$IFNDEF D10_4+}
function TStyledCategoryButtons.IsCustomStyleActive: Boolean;
begin
  Result := False;
end;
{$ENDIF}

function TStyledCategoryButtons.StyledButtonState(
  const AState: TButtonDrawState): TStyledButtonState;
begin
  //Calculate Styled State based on State
  if (bdsHot in AState) and not (bdsDown in AState) then
    Result := bsmHot
  else if bdsDown in AState then
    Result := bsmPressed
  else if bdsFocused in AState then
    Result := bsmSelected
  else if not Enabled then
    Result := bsmDisabled
  else
    Result := bsmNormal;
end;

procedure TStyledCategoryButtons.DrawButton(const AButton: TButtonItem;
  ACanvas: TCanvas; ARect: TRect; AState: TButtonDrawState);
var
  LSurfaceRect: TRect;
  LOldFontName: TFontName;
  LOldFontColor: TColor;
  LOldFontStyle: TFontStyles;
  LOldParentFont: boolean;
  LOldBrushStyle: TBrushStyle;
  LOldPenWidth: Integer;
  LStyle: TCustomStyleServices;
  LState: TStyledButtonState;
  LDetails: TThemedElementDetails;
  LButtonItem: TStyledButtonItem;
  LDropDownRect: TRect;
  LColor: TColor;
  LBadgeSize: TNotificationBadgeSize;
  LBadgePosition: TNotificationBadgePosition;
  LBadgeColor: TColor;
  LBadgeFontColor: TColor;
  LBadgeFontStyle: TFontStyles;
  LBadgeContent: string;
begin
  //Do not call inherited
  LButtonItem := AButton as TStyledButtonItem;
  Assert(Assigned(LButtonItem));

  if Assigned(OnDrawButton) and (not (csDesigning in ComponentState)) then
    OnDrawButton(Self, LButtonItem, ACanvas, ARect, AState)
  else
  begin
    if Assigned(OnBeforeDrawButton) then
      OnBeforeDrawButton(Self, LButtonItem, ACanvas, ARect, AState);

    LState := StyledButtonState(AState);

    LOldParentFont := ParentFont;
    LOldFontName := ACanvas.Font.Name;
    LOldFontColor := ACanvas.Font.Color;
    LOldFontStyle := ACanvas.Font.Style;
    LOldBrushStyle := ACanvas.Brush.Style;
    LOldPenWidth := ACanvas.Pen.Width;
    try
      GetDrawingStyle(ACanvas, LState, LButtonItem);

      //At the moment, no DropDown for Buttons
      LDropDownRect := TRect.Create(0,0,0,0);

      if FFlat then
      begin
        LStyle := StyleServices{$IFDEF D10_4+}(Self){$ENDIF};
        if (LState in [bsmDisabled, bsmNormal]) and IsStyleEnabled then
        begin
          if (bdsSelected in AState) or (bdsDown in AState) then
            LDetails := LStyle.GetElementDetails(tcbButtonSelected)
          else if bdsHot in AState then
            LDetails := LStyle.GetElementDetails(tcbButtonHot)
          else
            LDetails := LStyle.GetElementDetails(tcbButtonNormal);

          if not (IsCustomStyleActive and not (seFont in StyleElements)) and
             LStyle.GetElementColor(LDetails, ecTextColor, LColor) and (LColor <> clNone) then
            ACanvas.Font.Color := LColor;
        end;

        //Don't draw button Face for Flat Normal/disabled Button
        if LState in [bsmDisabled, bsmNormal] then
          ACanvas.Brush.Style := bsClear;
      end;

      DrawBackgroundAndBorder(ACanvas, ARect, LDropDownRect,
        LButtonItem.StyleDrawType, LButtonItem.StyleRadius, LButtonItem.StyleRoundedCorners);

      LSurfaceRect := ARect;
      if LDropDownRect.Width <> 0 then
        Dec(LSurfaceRect.Right, LDropDownRect.Width);

      DrawCaptionAndImage(ACanvas, LSurfaceRect, LButtonItem.Caption,
        LButtonItem.ImageIndex);

      { Draw the icon - prefer the event }
      if Assigned(OnDrawIcon) then
        OnDrawIcon(Self, LButtonItem, ACanvas, LSurfaceRect, AState, FSpacing);

      LSurfaceRect := ClientRect;

      //Get Notification Badge Infos by Event Handler
      if Assigned(FOnGetNotificationBadgeInfo) then
      begin
        LBadgeSize := nbsNormal;
        LBadgePosition := nbpTopRight;
        LBadgeColor := DEFAULT_BADGE_COLOR;
        LBadgeFontColor := DEFAULT_BADGE_FONT_COLOR;
        LBadgeContent := '';
        LBadgeFontStyle := [fsBold];
        FOnGetNotificationBadgeInfo(
          AButton.Category.Index, AButton.Index,
          LBadgeContent, LBadgeSize, LBadgePosition,
          LBadgeColor, LBadgeFontColor, LBadgeFontStyle);
        if LBadgeContent <> '' then
        begin
          DrawButtonNotificationBadge(ACanvas, ARect, GetScaleFactor,
            LBadgeContent, LBadgeSize, LBadgePosition,
            LBadgeColor, LBadgeFontColor, LBadgeFontStyle);
        end;
      end;
  (*
      { Show insert indications }
      if [bdsInsertLeft, bdsInsertTop, bdsInsertRight, bdsInsertBottom] * State <> [] then
      begin
        ACanvas.Brush.Color := GetShadowColor(EdgeColor);
        InsertIndication := Rect;
        if bdsInsertLeft in State then
        begin
          Dec(InsertIndication.Left, 2);
          InsertIndication.Right := InsertIndication.Left + 2;
        end
        else if bdsInsertTop in State then
        begin
          Dec(InsertIndication.Top);
          InsertIndication.Bottom := InsertIndication.Top + 2;
        end
        else if bdsInsertRight in State then
        begin
          Inc(InsertIndication.Right, 2);
          InsertIndication.Left := InsertIndication.Right - 2;
        end
        else if bdsInsertBottom in State then
        begin
          Inc(InsertIndication.Bottom);
          InsertIndication.Top := InsertIndication.Bottom - 2;
        end;
        ACanvas.FillRect(InsertIndication);
        ACanvas.Brush.Color := FillColor;
      end;
  *)

      if Assigned(OnAfterDrawButton) then
        OnAfterDrawButton(Self, LButtonItem, ACanvas, ARect, AState);
    finally
      //Restore original values
      ACanvas.Font.Name := LOldFontName;
      ACanvas.Font.Color := LOldFontColor;
      ACanvas.Font.Style := LOldFontStyle;
      ACanvas.Brush.Style := LOldBrushStyle;
      ACanvas.Pen.Width := LOldPenWidth;
      //ACanvas.Brush.Color := Color;
      if LOldParentFont then
        ParentFont := LOldParentFont;
    end;
  end;
end;

procedure TStyledCategoryButtons.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
end;

procedure TStyledCategoryButtons.ImageMarginsChange(Sender: TObject);
begin
  Invalidate;
end;

function TStyledCategoryButtons.IsDefaultImageMargins: Boolean;
begin
  //Default image margins: when all margins are zero except for
  case FImageAlignment of
    iaLeft: Result := (FImageMargins.Left = DEFAULT_IMAGE_HMARGIN) and
      (FImageMargins.Top = 0) and (FImageMargins.Right = 0) and (FImageMargins.Bottom = 0);
    iaRight: Result := (FImageMargins.Right = DEFAULT_IMAGE_HMARGIN) and
      (FImageMargins.Top = 0) and (FImageMargins.Left = 0) and (FImageMargins.Bottom = 0);
    iaTop: Result := (FImageMargins.Top = DEFAULT_IMAGE_VMARGIN) and
      (FImageMargins.Left = 0) and (FImageMargins.Right = 0) and (FImageMargins.Bottom = 0);
    iaBottom: Result := (FImageMargins.Bottom = DEFAULT_IMAGE_VMARGIN) and
      (FImageMargins.Left = 0) and (FImageMargins.Right = 0) and (FImageMargins.Top = 0);
    iaCenter: Result := (FImageMargins.Bottom = 0) and
      (FImageMargins.Left = 0) and (FImageMargins.Right = 0) and (FImageMargins.Top = 0);
  else
    Result := False;
  end;
end;

procedure TStyledCategoryButtons.CalcDefaultImageMargins(const AValue: TImageAlignment);

  function AdJustMargin(const AMargin, AOffset: Integer): Integer;
  begin
    Result := AMargin + Round(AOffset*GetScaleFactor);
  end;

begin
  if IsDefaultImageMargins then
  begin
    FImageMargins.Left := 0;
    FImageMargins.Right := 0;
    FImageMargins.Top := 0;
    FImageMargins.Bottom := 0;
    case AValue of
      iaLeft: FImageMargins.Left := AdJustMargin(FImageMargins.Left, DEFAULT_IMAGE_HMARGIN);
      iaRight: FImageMargins.Right := AdJustMargin(FImageMargins.Right, DEFAULT_IMAGE_HMARGIN);
      iaTop: FImageMargins.Top := AdJustMargin(FImageMargins.Top, DEFAULT_IMAGE_VMARGIN);
      iaBottom: FImageMargins.Bottom := AdJustMargin(FImageMargins.Bottom, DEFAULT_IMAGE_VMARGIN);
    end;
  end;
end;

function TStyledCategoryButtons.CalcMaxBorderWidth: Integer;
begin
  Result := Max(Max(Max(Max(FButtonStyleNormal.BorderWidth,
    FButtonStylePressed.BorderWidth),
    FButtonStyleSelected.BorderWidth),
    FButtonStyleHot.BorderWidth),
    FButtonStyleDisabled.BorderWidth);
end;

function TStyledCategoryButtons.ImageMarginsStored: Boolean;
begin
  Result := not IsDefaultImageMargins;
end;

function TStyledCategoryButtons.IsCustomDrawType: Boolean;
begin
  Result := FCustomDrawType;
end;

function TStyledCategoryButtons.IsCustomRoundedCorners: Boolean;
begin
  Result := StyleRoundedCorners <> ALL_ROUNDED_CORNERS;
end;

function TStyledCategoryButtons.IsCustomRadius: Boolean;
begin
  Result := StyleRadius <> DEFAULT_RADIUS;
end;

function TStyledCategoryButtons.IsStoredStyleAppearance: Boolean;
var
  LClass: TStyledButtonClass;
  LAppearance: TStyledButtonAppearance;
  LButtonFamily: TButtonFamily;
begin
  StyleFamilyCheckAttributes(FStyleFamily, LClass, LAppearance, LButtonFamily);
  Result := FStyleAppearance <> LAppearance;
end;

function TStyledCategoryButtons.IsStoredStyleClass: Boolean;
var
  LClass: TStyledButtonClass;
  LAppearance: TStyledButtonAppearance;
  LButtonFamily: TButtonFamily;
begin
  StyleFamilyCheckAttributes(FStyleFamily, LClass, LAppearance, LButtonFamily);

  if AsVCLStyle then
  begin
    Result := (FStyleClass <> GetActiveStyleName)
      and not SameText(FStyleClass, 'Windows');
  end
  else
    Result := FStyleClass <> LClass;
end;

function TStyledCategoryButtons.IsStoredStyleFamily: Boolean;
begin
  Result := FStyleFamily <> DEFAULT_CLASSIC_FAMILY;
end;

procedure TStyledCategoryButtons.Loaded;
begin
  inherited;
  if not FStyleApplied (*and not HasCustomAttributes*) then
  begin
    StyleFamilyUpdateAttributes(
      FStyleFamily, FStyleClass, FStyleAppearance,
      FButtonStyleNormal, FButtonStylePressed, FButtonStyleSelected,
      FButtonStyleHot, FButtonStyleDisabled);
    StyleApplied := ApplyButtonStyle;
  end;
end;

procedure TStyledCategoryButtons.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  LButtonItem: TButtonItem;
begin
  inherited;
  LButtonItem := GetButtonAt(X, Y);
  if Assigned(LButtonItem) then
    inherited Cursor := FButtonsCursor
  else
    inherited Cursor := FCursor;
end;

class procedure TStyledCategoryButtons.RegisterDefaultRenderingStyle(
  const ADrawType: TStyledButtonDrawType; const AFamily: TStyledButtonFamily;
  const AClass: TStyledButtonClass; const AAppearance: TStyledButtonAppearance;
  const AStyleRadius: Integer; const AButtonsCursor: TCursor);
begin
  _DefaultStyleDrawType := ADrawType;
  _UseCustomDrawType := True;
  _DefaultFamily := AFamily;
  _DefaultClass := AClass;
  _DefaultAppearance := AAppearance;
  _DefaultStyleRadius := AStyleRadius;
  _DefaultButtonsCursor := AButtonsCursor;
end;

function TStyledCategoryButtons.ApplyButtonStyle: Boolean;
var
  LButtonFamily: TButtonFamily;
  LStyleClass: TStyledButtonClass;
  LStyleAppearance: TStyledButtonAppearance;
begin
  if AsVCLStyle then
  begin
    //if StyleElements contains seClient then use
    //VCL Style assigned to Button or Global VCL Style
    if seBorder in StyleElements then
      LStyleAppearance := DEFAULT_APPEARANCE;
    LStyleClass := GetActiveStyleName;
  end
  else
  begin
    LStyleClass := FStyleClass;
    LStyleAppearance := FStyleAppearance;
  end;
  Result := StyleFamilyCheckAttributes(FStyleFamily,
    LStyleClass, LStyleAppearance, LButtonFamily);
  if Result (*or (csDesigning in ComponentState)*) then
  begin
    StyleFamilyUpdateAttributes(
      FStyleFamily,
      LStyleClass,
      LStyleAppearance,
      FButtonStyleNormal,
      FButtonStylePressed,
      FButtonStyleSelected,
      FButtonStyleHot,
      FButtonStyleDisabled);
  end;
  StyleClass := LStyleClass;
  StyleAppearance := LStyleAppearance;
  if Result then
    Invalidate;
end;

procedure TStyledCategoryButtons.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TStyledCategoryButtons then
  begin
    FStyleFamily := TStyledCategoryButtons(Source).FStyleFamily;
    FStyleClass := TStyledCategoryButtons(Source).FStyleClass;
    FStyleAppearance := TStyledCategoryButtons(Source).FStyleAppearance;
    FStyleRadius := TStyledCategoryButtons(Source).FStyleRadius;
    FStyleRoundedCorners := TStyledCategoryButtons(Source).FStyleRoundedCorners;
    FStyleDrawType := TStyledCategoryButtons(Source).FStyleDrawType;
    FButtonsCursor := TStyledCategoryButtons(Source).FButtonsCursor;
    Invalidate;
  end;
end;

function TStyledCategoryButtons.AsVCLStyle: Boolean;
begin
  Result := (StyleFamily = DEFAULT_CLASSIC_FAMILY) and
    (seClient in StyleElements);
end;

function TStyledCategoryButtons.GetAsVCLComponent: Boolean;
begin
  Result := (StyleFamily = DEFAULT_CLASSIC_FAMILY) and
    (seClient in StyleElements) and
    (FStyleClass = GetActiveStyleName);
end;

function TStyledCategoryButtons.GetButtonCategoriesClass: TButtonCategoriesClass;
begin
  Result := TStyledButtonCategories;
end;

function TStyledCategoryButtons.GetButtonCategoryClass: TButtonCategoryClass;
begin
  Result := TStyledButtonCategory;
end;

function TStyledCategoryButtons.GetButtonItemClass: TButtonItemClass;
begin
  Result := TStyledButtonItem;
end;

procedure TStyledCategoryButtons.SetAsVCLComponent(const AValue: Boolean);
begin
  if AValue <> GetAsVCLComponent then
  begin
    if AValue then
    begin
      FStyleFamily := DEFAULT_CLASSIC_FAMILY;
      FStyleClass := DEFAULT_WINDOWS_CLASS;
      FStyleAppearance := DEFAULT_APPEARANCE;
      StyleElements := StyleElements + [seClient];
      FCustomDrawType := False;
    end
    else if FStyleFamily = DEFAULT_CLASSIC_FAMILY then
    begin
      StyleElements := StyleElements - [seClient];
    end;
    UpdateStyleElements;
  end;
end;

procedure TStyledCategoryButtons.UpdateStyleElements;
var
  LStyleClass: TStyledButtonClass;
begin
  if AsVCLStyle then
  begin
    //if StyleElements contains seClient then Update style
    //as VCL Style assigned to CategoryButtons or Global VCL Style
    if seBorder in StyleElements then
      StyleAppearance := DEFAULT_APPEARANCE;
    LStyleClass := GetActiveStyleName;
    FStyleClass := LStyleClass;
    StyleApplied := ApplyCategoryButtonsStyle;
  ProcessButtons(
    procedure (ABtn: TStyledButtonItem)
    begin
      //ABtn.UpdateStyleElements;
      ABtn.StyleDrawType := Self.StyleDrawType;
    end);
  end;
  inherited;
end;

function TStyledCategoryButtons.ApplyCategoryButtonsStyle: Boolean;
var
  LButtonFamily: TButtonFamily;
begin
  Result := StyleFamilyCheckAttributes(FStyleFamily,
    FStyleClass, FStyleAppearance, LButtonFamily);
  if Result or (csDesigning in ComponentState) then
  begin
    StyleFamilyUpdateAttributes(
      FStyleFamily,
      FStyleClass,
      FstyleAppearance,
      FButtonStyleNormal,
      FButtonStylePressed,
      FButtonStyleSelected,
      FButtonStyleHot,
      FButtonStyleDisabled);
  end;
  Invalidate;
end;

procedure TStyledCategoryButtons.SetSpacing(const AValue: Integer);
begin
  if FSpacing <> AValue then
  begin
    FSpacing := AValue;
    Invalidate;
  end;
end;

procedure TStyledCategoryButtons.SetStyleAppearance(
  const AValue: TStyledButtonAppearance);
var
  LValue: TStyledButtonAppearance;
begin
  LValue := AValue;
  if LValue = '' then
    LValue := DEFAULT_APPEARANCE;
  if (FStyleAppearance <> LValue) or not FStyleApplied then
  begin
    if not (csLoading in ComponentState) then
    begin
      ProcessButtons(
        procedure (ABtn: TStyledButtonItem)
        begin
          if ABtn.StyleAppearance = StyleAppearance then
            ABtn.StyleAppearance := LValue;
        end);
    end;
    FStyleAppearance := LValue;
    StyleApplied := ApplyCategoryButtonsStyle;
  end;
end;

procedure TStyledCategoryButtons.SetStyleApplied(const AValue: Boolean);
begin
  FStyleApplied := AValue and (Categories.Count > 0) and
    not (csLoading in ComponentState);
  if FStyleApplied then
  begin
    if not (csLoading in ComponentState) then
    begin
      ProcessButtons(
        procedure (ABtn: TStyledButtonItem)
        begin
          ABtn.LoadDefaultStyles;
        end);
    end;
  end;
end;

procedure TStyledCategoryButtons.SetStyleClass(const AValue: TStyledButtonClass);
var
  LValue: TStyledButtonClass;
begin
  LValue := AValue;
  if LValue = '' then
    LValue := DEFAULT_WINDOWS_CLASS;
  if (LValue <> Self.FStyleClass) or not FStyleApplied then
  begin
    if not (csLoading in ComponentState) then
    begin
      ProcessButtons(
        procedure (ABtn: TStyledButtonItem)
        begin
          if ABtn.StyleClass = StyleClass then
            ABtn.StyleClass := LValue;
        end);
    end;
    FStyleClass := LValue;
    StyleApplied := ApplyCategoryButtonsStyle;
//    if (FStyleFamily = DEFAULT_CLASSIC_FAMILY) and
//      (LValue <> 'Windows') then
//      StyleElements := [seFont, seBorder];
  end;
end;

procedure TStyledCategoryButtons.SetStyleDrawType(const AValue: TStyledButtonDrawType);
begin
  FCustomDrawType := True;
  if FStyleDrawType <> AValue then
  begin
    ProcessButtons(
      procedure (ABtn: TStyledButtonItem)
      begin
        if ABtn.StyleDrawType = StyleDrawType then
          ABtn.StyleDrawType := AValue;
      end);
    FStyleDrawType := AValue;
    StyleApplied := ApplyCategoryButtonsStyle;
  end;
end;

procedure TStyledCategoryButtons.SetStyleFamily(const AValue: TStyledButtonFamily);
var
  LValue: TStyledButtonFamily;
begin
  LValue := AValue;
  if LValue = '' then
    LValue := DEFAULT_CLASSIC_FAMILY;
  if (LValue <> Self.FStyleFamily) or not FStyleApplied then
  begin
    if not (csLoading in ComponentState) then
    begin
      ProcessButtons(
        procedure (ABtn: TStyledButtonItem)
        begin
          if ABtn.StyleFamily = FStyleFamily then
            ABtn.StyleFamily := LValue;
        end);
    end;
    FStyleFamily := LValue;
    StyleApplied := ApplyCategoryButtonsStyle;
  end;
  if FStyleFamily = DEFAULT_CLASSIC_FAMILY then
    StyleElements := [seFont, seClient, seBorder];
end;

procedure TStyledCategoryButtons.SetStyleRadius(const AValue: Integer);
begin
  if FStyleRadius <> AValue then
  begin
    if AValue <= 0 then
      raise EReadError.create(SInvalidProperty);
    ProcessButtons(
      procedure (ABtn: TStyledButtonItem)
      begin
        if ABtn.StyleRadius = FStyleRadius then
          ABtn.StyleRadius := AValue;
      end);
    FStyleRadius := AValue;
    StyleApplied := ApplyCategoryButtonsStyle;
  end;
end;

procedure TStyledCategoryButtons.SetStyleRoundedCorners(
  const AValue: TRoundedCorners);
begin
  if FStyleRoundedCorners <> AValue then
  begin
    ProcessButtons(
      procedure (ABtn: TStyledButtonItem)
      begin
        if ABtn.StyleRoundedCorners = FStyleRoundedCorners then
          ABtn.StyleRoundedCorners := AValue;
      end);
    FStyleRoundedCorners := AValue;
    StyleApplied := ApplyCategoryButtonsStyle;
  end;
end;

procedure TStyledCategoryButtons.SetCategoryButtonsStyle(
  const AStyleFamily: TStyledButtonFamily;
  const AStyleClass: TStyledButtonClass;
  const AStyleAppearance: TStyledButtonAppearance);
begin
  StyleFamily := AStyleFamily;
  StyleClass := AStyleClass;
  StyleAppearance := AStyleAppearance;
  if not ApplyCategoryButtonsStyle then
    raise EStyledCategoryButtonsError.CreateFmt(ERROR_SETTING_CATEGORYBUTTONS_STYLE,
      [AStyleFamily, AStyleClass, AStyleAppearance]);
end;

procedure TStyledCategoryButtons.SetButtonStyleDisabled(
  const AValue: TStyledButtonAttributes);
begin
  if FButtonStyleDisabled <> AValue then
  begin
    FButtonStyleDisabled := AValue;
    Invalidate;
  end;
end;

procedure TStyledCategoryButtons.SetButtonStyleHot(
  const AValue: TStyledButtonAttributes);
begin
  if FButtonStyleHot <> AValue then
  begin
    FButtonStyleHot := AValue;
    Invalidate;
  end;
end;

procedure TStyledCategoryButtons.SetButtonStyleNormal(
  const AValue: TStyledButtonAttributes);
begin
  if FButtonStyleNormal <> AValue then
  begin
    FButtonStyleNormal := AValue;
    Invalidate;
  end;
end;

procedure TStyledCategoryButtons.SetButtonStylePressed(
  const AValue: TStyledButtonAttributes);
begin
  if FButtonStylePressed <> AValue then
  begin
    FButtonStylePressed := AValue;
    Invalidate;
  end;
end;

procedure TStyledCategoryButtons.SetButtonStyleSelected(
  const AValue: TStyledButtonAttributes);
begin
  if FButtonStyleSelected <> AValue then
  begin
    FButtonStyleSelected := AValue;
    Invalidate;
  end;
end;

procedure TStyledCategoryButtons.SetCaptionAlignment(const AValue: TAlignment);
begin
  if FCaptionAlignment <> AValue then
  begin
    FCaptionAlignment := AValue;
    Invalidate;
  end;
end;

procedure TStyledCategoryButtons.SetCursor(const AValue: TCursor);
begin
  inherited Cursor := AValue;
  FCursor := AValue;
end;

procedure TStyledCategoryButtons.SetFlat(const AValue: Boolean);
begin
  if FFlat <> AValue then
  begin
    FFlat := AValue;
    Invalidate;
  end;
end;

procedure TStyledCategoryButtons.SetButtonCategories(const AValue: TStyledButtonCategories);
begin
  inherited Categories := AValue;
end;

procedure TStyledCategoryButtons.SetImageAlignment(const AValue: TImageAlignment);
begin
  if FImageAlignment <> AValue then
  begin
    CalcDefaultImageMargins(AValue);
    FImageAlignment := AValue;
    if Images <> nil then
      Invalidate;
  end;
end;

procedure TStyledCategoryButtons.SetImageMargins(const AValue: TImageMargins);
begin
  FImageMargins.Assign(AValue);
end;

function TStyledCategoryButtons.GetActiveStyleName: string;
begin
  Result := Vcl.ButtonStylesAttributes.GetActiveStyleName(Self);
end;

function TStyledCategoryButtons.GetAttributes(const AMode: TStyledButtonState): TStyledButtonAttributes;
begin
  case Amode of
    bsmPressed: Result := FButtonStylePressed;
    bsmSelected: Result := FButtonStyleSelected;
    bsmHot: Result := FButtonStyleHot;
    bsmDisabled: Result := FButtonStyleDisabled;
  else
    Result := FButtonStyleNormal;
  end;
end;

procedure TStyledCategoryButtons.GetDrawingStyle(const ACanvas: TCanvas;
  const AButtonState: TStyledButtonState;
  const AItem: TStyledButtonItem);
var
  LCustomAttributes: Boolean;
  LSyleAttributes: TStyledButtonAttributes;
begin
  LCustomAttributes := ((AItem.FStyleFamily) <> FStyleFamily) or
    ((AItem.FStyleClass) <> FStyleClass) or
    ((AItem.FStyleAppearance) <> FStyleAppearance);
  try
    if LCustomAttributes then
    begin
      //Getting custom drawing styles for single button
      StyleFamilyUpdateAttributes(
        AItem.FStyleFamily, AItem.FStyleClass, AItem.FStyleAppearance,
        FButtonStyleNormal, FButtonStylePressed, FButtonStyleSelected,
        FButtonStyleHot, FButtonStyleDisabled);
    end;

    //Getting custom drawing styles for all buttons
    LSyleAttributes := GetAttributes(AButtonState);

    ACanvas.Pen.Style := LSyleAttributes.PenStyle;
    ACanvas.Pen.Width := Round(LSyleAttributes.BorderWidth{$IFDEF D10_3+}*ScaleFactor{$ENDIF});
    ACanvas.Pen.Color := LSyleAttributes.BorderColor;
    ACanvas.Brush.Style := LSyleAttributes.BrushStyle;
    if LSyleAttributes.ButtonDrawStyle <> btnClear then
      ACanvas.Brush.Color := LSyleAttributes.ButtonColor;
    ACanvas.Font := Font;
    ACanvas.Font.Color := LSyleAttributes.FontColor;
    if ParentFont then
      ACanvas.Font.Style := LSyleAttributes.FontStyle;
  finally
    if LCustomAttributes then
    begin
      //Restore drawing styles for every buttons
      StyleFamilyUpdateAttributes(
        FStyleFamily, FStyleClass, FStyleAppearance,
        FButtonStyleNormal, FButtonStylePressed, FButtonStyleSelected,
        FButtonStyleHot, FButtonStyleDisabled);
    end;
  end;
end;

function TStyledCategoryButtons.GetButtonCategories: TStyledButtonCategories;
begin
  Result := inherited Categories as TStyledButtonCategories;
end;

function TStyledCategoryButtons.IsStyleEnabled: Boolean;
begin
  Result := IsCustomStyleActive and (StyleElements <> []);
end;

procedure TStyledCategoryButtons.ProcessButtons(
  AButtonProc: TButtonProc);
var
  C, B: Integer;
  LCategory: TStyledButtonCategory;
  LButton: TStyledButtonItem;
begin
  if not Assigned(Categories) then
    Exit;
  for C := 0 to Categories.Count -1 do
  begin
    if Categories[C] is TStyledButtonCategory then
    begin
      LCategory := TStyledButtonCategory(Categories[C]);
      for B := 0 to LCategory.Items.Count -1 do
      begin
        if LCategory.Items[B] is TStyledButtonItem then
        begin
          LButton := TStyledButtonItem(LCategory.Items[B]);
          AButtonProc(LButton);
        end;
      end;
    end;
  end;
end;

{ TStyledButtonItem }

function TStyledButtonItem.ApplyButtonStyle: Boolean;
var
  LButtonFamily: TButtonFamily;
begin
  if not FStyleApplied then
  begin
    Result := StyleFamilyCheckAttributes(FStyleFamily,
      FStyleClass, FStyleAppearance, LButtonFamily);
    if Result then
    begin
      InvalidateOwner;
      FStyleApplied := True;
    end;
  end
  else
    Result := False;
end;

procedure TStyledButtonItem.LoadDefaultStyles;
begin
  if Assigned(CategoryButtons) and (CategoryButtons.FStyleApplied) and not FStyleApplied then
  begin
    FStyleFamily := CategoryButtons.StyleFamily;
    FStyleClass := CategoryButtons.StyleClass;
    FStyleAppearance := CategoryButtons.StyleAppearance;
    FStyleRadius := CategoryButtons.StyleRadius;
    FStyleRoundedCorners := CategoryButtons.StyleRoundedCorners;
    FStyleDrawType := CategoryButtons.StyleDrawType;
  end;
end;

procedure TStyledButtonItem.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TStyledButtonItem then
  begin
    FStyleRadius := TStyledButtonItem(Source).StyleRadius;
    FStyleDrawType := TStyledButtonItem(Source).StyleDrawType;
    FStyleRoundedCorners := TStyledButtonItem(Source).StyleRoundedCorners;
    FStyleFamily := TStyledButtonItem(Source).StyleFamily;
    FStyleClass := TStyledButtonItem(Source).StyleClass;
    FStyleAppearance := TStyledButtonItem(Source).StyleAppearance;
  end;
end;

constructor TStyledButtonItem.Create(Collection: TCollection);
begin
  inherited;
  FCollection := Collection as TButtonCollection;
  FStyleDrawType := TStyledCategoryButtons._DefaultStyleDrawType;
  FStyleRadius := TStyledCategoryButtons._DefaultStyleRadius;
  FStyleRoundedCorners := ALL_ROUNDED_CORNERS;
  LoadDefaultStyles;
end;

function TStyledButtonItem.GetStyledCategoryButtons: TStyledCategoryButtons;
begin
  if Assigned(FCollection) and Assigned(FCollection.Category) then
    Result := FCollection.Category.CategoryButtons as TStyledCategoryButtons
  else
    Result := nil;
end;

procedure TStyledButtonItem.InvalidateOwner;
begin
  if Assigned(CategoryButtons) then
    CategoryButtons.Invalidate;
end;

function TStyledButtonItem.IsCustomDrawType: Boolean;
begin
  Result := Assigned(CategoryButtons) and
    (FStyleDrawType <> CategoryButtons.FStyleDrawType);
end;

function TStyledButtonItem.IsCustomRoundedCorners: Boolean;
begin
  Result := Assigned(CategoryButtons) and
    (FStyleRoundedCorners <> CategoryButtons.FStyleRoundedCorners);
end;

function TStyledButtonItem.IsCustomRadius: Boolean;
begin
  Result := Assigned(CategoryButtons) and
    (FStyleRadius <> CategoryButtons.FStyleRadius);
end;

function TStyledButtonItem.IsStoredStyle: Boolean;
begin
  Result := Assigned(CategoryButtons) and
    ((FStyleFamily <> CategoryButtons.FStyleFamily) or
     (FStyleClass <> CategoryButtons.FStyleClass) or
     (FStyleAppearance <> CategoryButtons.FStyleAppearance));
end;

procedure TStyledButtonItem.SetStyleFamily(
  const AValue: TStyledButtonFamily);
begin
  if FStyleFamily <> AValue then
  begin
    FStyleFamily := AValue;
    InvalidateOwner;
  end;
  ApplyButtonStyle;
end;

procedure TStyledButtonItem.SetStyleClass(
  const AValue: TStyledButtonClass);
begin
  if FStyleClass <> AValue then
  begin
    FStyleClass := AValue;
    InvalidateOwner;
  end;
  ApplyButtonStyle;
end;

procedure TStyledButtonItem.SetButtonStyle(
  const AStyleFamily: TStyledButtonFamily;
  const AStyleClass: TStyledButtonClass;
  const AStyleAppearance: TStyledButtonAppearance);
begin
  FStyleApplied := False;
  FStyleFamily := AStyleFamily;
  FStyleClass := AStyleClass;
  FStyleAppearance := AStyleAppearance;
  if not ApplyButtonStyle then
    raise EStyledCategoryButtonsError.CreateFmt(ERROR_SETTING_BUTTON_STYLE,
      [AStyleFamily, AStyleClass, AStyleAppearance]);
end;

procedure TStyledButtonItem.SetStyleAppearance(
  const AValue: TStyledButtonAppearance);
begin
  if FStyleAppearance <> AValue then
  begin
    FStyleAppearance := AValue;
    InvalidateOwner;
  end;
  ApplyButtonStyle;
end;

procedure TStyledButtonItem.SetStyleDrawType(
  const AValue: TStyledButtonDrawType);
begin
  if FStyleDrawType <> AValue then
  begin
    FStyleDrawType := AValue;
    InvalidateOwner;
  end;
  ApplyButtonStyle;
end;

procedure TStyledButtonItem.SetStyleRadius(const AValue: Integer);
begin
  if FStyleRadius <> AValue then
  begin
    FStyleRadius := AValue;
    InvalidateOwner;
  end;
  ApplyButtonStyle;
end;

procedure TStyledButtonItem.SetStyleRoundedCorners(const AValue: TRoundedCorners);
begin
  if FStyleRoundedCorners <> AValue then
  begin
    FStyleRoundedCorners := AValue;
    InvalidateOwner;
  end;
  ApplyButtonStyle;
end;

{ TStyledButtonCategories }

function TStyledButtonCategories.Add: TStyledButtonCategory;
begin
  Result := inherited Add as TStyledButtonCategory;
end;

constructor TStyledButtonCategories.Create(const CategoryButtons: TCategoryButtons);
begin
  inherited Create(CategoryButtons);
end;

function TStyledButtonCategories.GetItem(Index: Integer): TStyledButtonCategory;
begin
  Result := inherited GetItem(Index) as TStyledButtonCategory;
end;

procedure TStyledButtonCategories.SetItem(Index: Integer;
  const AValue: TStyledButtonCategory);
begin
  inherited SetItem(Index, AValue);
end;

function TStyledButtonCategories.GetStyledCategoryButtons: TStyledCategoryButtons;
begin
  Result := inherited CategoryButtons as TStyledCategoryButtons;
end;

{ TStyledButtonCategory }

constructor TStyledButtonCategory.Create(Collection: TCollection);
begin
  inherited Create(Collection);
end;

initialization
  TStyledCategoryButtons._DefaultStyleDrawType := DEFAULT_STYLEDRAWTYPE;
  TStyledCategoryButtons._DefaultFamily := DEFAULT_CLASSIC_FAMILY;
  TStyledCategoryButtons._DefaultClass := DEFAULT_WINDOWS_CLASS;
  TStyledCategoryButtons._DefaultAppearance := DEFAULT_APPEARANCE;
  TStyledCategoryButtons._DefaultStyleRadius := DEFAULT_RADIUS;
  TStyledCategoryButtons._DefaultButtonsCursor := DEFAULT_CURSOR;

end.
