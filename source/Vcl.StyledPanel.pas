{******************************************************************************}
{                                                                              }
{  TStyledPanel: a "styled" Panel based on TCustomPanel                        }
{                                                                              }
{  Copyright (c) 2022-2026 (Ethea S.r.l.)                                      }
{  Author: Carlo Barazzetta                                                    }
{  Contributors: Claude Code                                                   }
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
unit Vcl.StyledPanel;

interface

{$INCLUDE StyledComponents.inc}

uses
  System.SysUtils
  , System.Classes
  , System.UITypes
  , Winapi.Windows
  , Winapi.Messages
  , Vcl.Graphics
  , Vcl.Controls
  , Vcl.ExtCtrls
  , Vcl.Themes
  , Vcl.ButtonStylesAttributes
  , Vcl.StandardButtonStyles
  ;

resourcestring
  ERROR_SETTING_PANEL_STYLE = 'Error setting Panel Style: %s/%s/%s not available';

const
  DEFAULT_CAPTION_MARGIN = 4;

type
  /// <summary>Exception class for styled panel errors</summary>
  EStyledPanelError = Exception;
  TStyledPanel = class;
  /// <summary>Class reference type for TStyledPanel descendants</summary>
  TStyledPanelClass = class of TStyledPanel;

  /// <summary>Styled panel component with modern appearance</summary>
  /// <remarks>
  ///   TStyledPanel is a styled alternative to TPanel that provides modern
  ///   visual appearance through the StyledComponents styling system.
  ///   Unlike button components, TStyledPanel uses TStyledButtonAttributes
  ///   directly for Normal and Disabled states (no TStyledButtonRender).
  ///   Supports custom styles via StyleFamily, StyleClass, StyleAppearance,
  ///   rounded corners, and all standard TPanel features. Can act as a
  ///   container for other controls. Switch to standard VCL rendering
  ///   via AsVCLComponent property. Use RegisterDefaultRenderingStyle
  ///   to set global defaults for all styled panels.
  /// </remarks>
  [ComponentPlatforms(pidWin32 or pidWin64)]
  TStyledPanel = class(TCustomPanel)
  private
    //Styled Attributes for two states
    FPanelStyleNormal: TStyledButtonAttributes;
    FPanelStyleDisabled: TStyledButtonAttributes;

    //Styled Attributes
    FStyleRadius: Integer;
    FStyleRoundedCorners: TRoundedCorners;
    FStyleDrawType: TStyledButtonDrawType;
    FStyleFamily: TStyledButtonFamily;
    FStyleClass: TStyledButtonClass;
    FStyleAppearance: TStyledButtonAppearance;
    FStyleApplied: Boolean;
    FCustomDrawType: Boolean;

    //Caption Rendering
    FCaptionAlignment: TAlignment;
    FCaptionMargin: Integer;

    //Default values
    class var _DefaultStyleDrawType: TStyledButtonDrawType;
    class var _DefaultFamily: TStyledButtonFamily;
    class var _DefaultClass: TStyledButtonClass;
    class var _DefaultAppearance: TStyledButtonAppearance;
    class var _DefaultStyleRadius: Integer;

    procedure SetStyleRadius(const AValue: Integer);
    procedure SetStyleRoundedCorners(const AValue: TRoundedCorners);
    procedure SetStyleDrawType(const AValue: TStyledButtonDrawType);
    procedure SetStyleFamily(const AValue: TStyledButtonFamily);
    procedure SetStyleClass(const AValue: TStyledButtonClass);
    procedure SetStyleAppearance(const AValue: TStyledButtonAppearance);
    procedure SetCaptionAlignment(const AValue: TAlignment);
    function IsCustomRoundedCorners: Boolean;
    function IsCustomRadius: Boolean;
    function IsStoredColor: Boolean;
    function IsStoredParentBackground: Boolean;
    /// <summary>Determines if StyleFamily should be stored</summary>
    function IsStoredStyleFamily: Boolean;
    /// <summary>Determines if StyleClass should be stored</summary>
    function IsStoredStyleClass: Boolean;
    /// <summary>Determines if StyleAppearance should be stored</summary>
    function IsStoredStyleAppearance: Boolean;
    /// <summary>Determines if StyleElements should be stored</summary>
    function IsStoredStyleElements: Boolean;
    function ApplyPanelStyle: Boolean;
    function GetAttributes(const AEnabled: Boolean): TStyledButtonAttributes;
    procedure CMStyleChanged(var AMessage: TMessage); message CM_STYLECHANGED;
    procedure SetCaptionMargin(const AValue: Integer);
    function GetAsVCLComponent: Boolean;
    procedure SetAsVCLComponent(const AValue: Boolean);
    function AsVCLStyle: Boolean;
    function GetActiveStyleName: string;
    function GetColor: TColor;
    procedure SetColor(const AValue: TColor);
    procedure SetPanelStyleDisabled(const AValue: TStyledButtonAttributes);
    procedure SetPanelStyleNormal(const AValue: TStyledButtonAttributes);
  protected
    procedure Paint; override;
    procedure Loaded; override;
    procedure UpdateStyleElements; override;
    {$IFNDEF D10_4+}
    function IsCustomStyleActive: Boolean;
    {$ENDIF}
  public
    /// <summary>Checks if a custom draw type has been set</summary>
    function IsCustomDrawType: Boolean;
    /// <summary>Sets whether a custom draw type is being used</summary>
    procedure SetCustomStyleDrawType(ACustomStyleDrawType: Boolean);
    {$IFDEF D10_4+}
    /// <summary>Scales panel for new PPI (High DPI support)</summary>
    procedure ScaleForPPI(NewPPI: Integer); override;
    {$ENDIF}
    {$IFDEF D10_1+}
    /// <summary>Handles DPI scaling changes</summary>
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    {$ENDIF}
    /// <summary>Copies properties from another panel</summary>
    procedure Assign(Source: TPersistent); override;
    /// <summary>Copies Styled properties to another Panel</summary>
    procedure AssignStyleTo(Dest: TPersistent);
    /// <summary>Creates panel with default style settings</summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Creates panel with specified style settings</summary>
    /// <param name="AOwner">Component owner</param>
    /// <param name="AFamily">Initial style family</param>
    /// <param name="AClass">Initial style class</param>
    /// <param name="AAppearance">Initial style appearance</param>
    constructor CreateStyled(AOwner: TComponent;
      const AFamily: TStyledButtonFamily;
      const AClass: TStyledButtonClass;
      const AAppearance: TStyledButtonAppearance); virtual;
    /// <summary>Destroys the panel and releases resources</summary>
    destructor Destroy; override;
    /// <summary>Sets the panel style at runtime</summary>
    /// <param name="AStyleFamily">Style family name</param>
    /// <param name="AStyleClass">Style class within the family</param>
    /// <param name="AStyleAppearance">Style appearance variant</param>
    procedure SetPanelStyle(const AStyleFamily: TStyledButtonFamily;
      const AStyleClass: TStyledButtonClass;
      const AStyleAppearance: TStyledButtonAppearance);
    /// <summary>Registers default rendering style for all new panel instances</summary>
    /// <param name="ADrawType">Default draw type (shape)</param>
    /// <param name="AFamily">Default style family</param>
    /// <param name="AClass">Default style class</param>
    /// <param name="AAppearance">Default style appearance</param>
    /// <param name="AStyleRadius">Default corner radius</param>
    class procedure RegisterDefaultRenderingStyle(
      const ADrawType: TStyledButtonDrawType;
      const AFamily: TStyledButtonFamily = DEFAULT_CLASSIC_FAMILY;
      const AClass: TStyledButtonClass = DEFAULT_WINDOWS_CLASS;
      const AAppearance: TStyledButtonAppearance = DEFAULT_APPEARANCE;
      const AStyleRadius: Integer = DEFAULT_RADIUS); virtual;
  published
    /// <summary>Returns the currently active VCL style name</summary>
    property ActiveStyleName: string read GetActiveStyleName;
    /// <summary>Custom attributes for normal button state</summary>
    property PanelStyleNormal: TStyledButtonAttributes read FPanelStyleNormal write SetPanelStyleNormal;
    /// <summary>Custom attributes for disabled button state</summary>
    property PanelStyleDisabled: TStyledButtonAttributes read FPanelStyleDisabled write SetPanelStyleDisabled;

    //Publishing TCustomPanel properties
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter default bvNone;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property Color: TColor read GetColor write SetColor stored IsStoredColor;
    property Constraints;
    property Ctl3D;
    property UseDockManager default True;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Font;
    {$IFDEF D10_4+}
    property Padding;
    {$ENDIF}
    property ParentBiDiMode;
    property ParentBackground stored IsStoredParentBackground;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property ShowCaption;
    property TabOrder;
    property TabStop;
    property Touch;
    property VerticalAlignment;
    property Visible;
    property StyleElements stored IsStoredStyleElements;
    {$IFDEF D10_3+}
    property OnAlignInsertBefore;
    property OnAlignPosition;
    {$ENDIF}
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnGetSiteInfo;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;

    //StyledComponents Attributes
    property AsVCLComponent: Boolean read GetAsVCLComponent write SetAsVCLComponent stored False;
    property CaptionAlignment: TAlignment read FCaptionAlignment write SetCaptionAlignment default taCenter;
    property CaptionMargin: Integer read FCaptionMargin write SetCaptionMargin default DEFAULT_CAPTION_MARGIN;
    property StyleRadius: Integer read FStyleRadius write SetStyleRadius stored IsCustomRadius;
    property StyleDrawType: TStyledButtonDrawType read FStyleDrawType write SetStyleDrawType stored IsCustomDrawType;
    property StyleRoundedCorners: TRoundedCorners read FStyleRoundedCorners write SetStyleRoundedCorners stored IsCustomRoundedCorners;
    property StyleFamily: TStyledButtonFamily read FStyleFamily write SetStyleFamily stored IsStoredStyleFamily;
    property StyleClass: TStyledButtonClass read FStyleClass write SetStyleClass stored IsStoredStyleClass;
    property StyleAppearance: TStyledButtonAppearance read FStyleAppearance write SetStyleAppearance stored IsStoredStyleAppearance;
  end;

implementation

uses
  Vcl.Forms
  , System.Math
  ;

{ TStyledPanel }

constructor TStyledPanel.Create(AOwner: TComponent);
begin
  CreateStyled(AOwner,
    _DefaultFamily,
    _DefaultClass,
    _DefaultAppearance);
end;

constructor TStyledPanel.CreateStyled(AOwner: TComponent;
  const AFamily: TStyledButtonFamily;
  const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance);
begin
  Assert(Assigned(AOwner));
  inherited Create(AOwner);

  //Create Attributes
  FPanelStyleNormal := TStyledButtonAttributes.Create(Self);
  FPanelStyleNormal.Name := 'PanelStyleNormal';
  FPanelStyleDisabled := TStyledButtonAttributes.Create(Self);
  FPanelStyleDisabled.Name := 'PanelStyleDisabled';

  //Style initialization
  FStyleDrawType := _DefaultStyleDrawType;
  FCustomDrawType := True;
  FStyleRadius := _DefaultStyleRadius;
  FStyleRoundedCorners := ALL_ROUNDED_CORNERS;
  FStyleFamily := AFamily;
  FStyleAppearance := AAppearance;
  FStyleApplied := True;

  //Caption defaults
  FCaptionAlignment := taCenter;
  FCaptionMargin := DEFAULT_CAPTION_MARGIN;

  //Different default
  BevelOuter := bvNone;
  ParentBackground := not IsStoredParentBackground;

  //Call the Setter of StyleClass!
  StyleClass := AClass;
end;

destructor TStyledPanel.Destroy;
begin
  FPanelStyleNormal.Free;
  FPanelStyleDisabled.Free;
  inherited Destroy;
end;

procedure TStyledPanel.AssignStyleTo(Dest: TPersistent);
var
  LDestPanel: TStyledPanel;
begin
  inherited;
  if Dest is TStyledPanel then
  begin
    LDestPanel := TStyledPanel(Dest);
    LDestPanel.FStyleRadius := FStyleRadius;
    LDestPanel.FStyleRoundedCorners := FStyleRoundedCorners;
    LDestPanel.FStyleDrawType := FStyleDrawType;
    LDestPanel.FStyleFamily := FStyleFamily;
    LDestPanel.FStyleAppearance := FStyleAppearance;
    LDestPanel.FCaptionAlignment := FCaptionAlignment;
    LDestPanel.FStyleApplied := Self.FStyleApplied;
    LDestPanel.FCustomDrawType := Self.FCustomDrawType;
    //Call Setter
    LDestPanel.StyleClass := FStyleClass;
  end;
end;

procedure TStyledPanel.Assign(Source: TPersistent);
var
  LSourcePanel: TStyledPanel;
begin
  //Ancestor classes do not declare Assign, so a bare 'inherited' falls through to
  //TPersistent.AssignTo -> AssignError (EConvertError) before the styled state is
  //copied. Only call 'inherited' when Source is NOT a TStyledPanel.
  if Source is TStyledPanel then
  begin
    LSourcePanel := TStyledPanel(Source);
    FStyleRadius := LSourcePanel.FStyleRadius;
    FStyleRoundedCorners := LSourcePanel.FStyleRoundedCorners;
    FStyleDrawType := LSourcePanel.FStyleDrawType;
    FStyleFamily := LSourcePanel.FStyleFamily;
    FStyleAppearance := LSourcePanel.FStyleAppearance;
    FCaptionAlignment := LSourcePanel.FCaptionAlignment;
    FStyleApplied := LSourcePanel.FStyleApplied;
    StyleClass := LSourcePanel.FStyleClass;
  end
  else
    inherited;
end;

function TStyledPanel.ApplyPanelStyle: Boolean;
var
  LDummyPressed, LDummySelected, LDummyHot: TStyledButtonAttributes;
  LButtonFamily: TButtonFamily;
  LStyleClass: TStyledButtonClass;
  LStyleAppearance: TStyledButtonAppearance;
begin
  if AsVCLStyle then
  begin
    //if StyleElements contains seBorder then use
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
  LDummyPressed := TStyledButtonAttributes.Create(nil);
  LDummySelected := TStyledButtonAttributes.Create(nil);
  LDummyHot := TStyledButtonAttributes.Create(nil);
  try
    Result := StyleFamilyCheckAttributes(FStyleFamily,
      LStyleClass, LStyleAppearance, LButtonFamily);
    if Result (*or (csDesigning in ComponentState)*) then
    begin
      StyleFamilyUpdateAttributes(
        FStyleFamily,
        FStyleClass,
        FStyleAppearance,
        FPanelStyleNormal,
        LDummyPressed,
        LDummySelected,
        LDummyHot,
        FPanelStyleDisabled);
      Color := FPanelStyleNormal.ButtonColor;
      if not FCustomDrawType then
        FStyleDrawType := FPanelStyleNormal.DrawType;
    end
    else
    begin
      FStyleClass := LStyleClass;
      FStyleAppearance := LStyleAppearance;
    end;
  finally
    LDummyPressed.Free;
    LDummySelected.Free;
    LDummyHot.Free;
  end;
  if Result then
    Invalidate;
end;

function TStyledPanel.GetAttributes(const AEnabled: Boolean): TStyledButtonAttributes;
begin
  if AEnabled then
    Result := FPanelStyleNormal
  else
    Result := FPanelStyleDisabled;
end;

{$IFDEF D10_1+}
procedure TStyledPanel.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  inherited;
  FStyleRadius := MulDiv(FStyleRadius, M, D);
  Invalidate;
end;
{$ENDIF}

{$IFDEF D10_4+}
procedure TStyledPanel.ScaleForPPI(NewPPI: Integer);
begin
  inherited;
  Invalidate;
end;
{$ENDIF}

procedure TStyledPanel.CMStyleChanged(var AMessage: TMessage);
begin
  inherited;
  ApplyPanelStyle;
end;

procedure TStyledPanel.UpdateStyleElements;
begin
  inherited;
  Invalidate;
end;

{$IFNDEF D10_4+}
function TStyledPanel.IsCustomStyleActive: Boolean;
begin
  Result := TStyleManager.IsCustomStyleActive;
end;
{$ENDIF}

procedure TStyledPanel.Loaded;
begin
  inherited;
  if not FStyleApplied then
    FStyleApplied := ApplyPanelStyle;
end;

procedure TStyledPanel.Paint;
var
  LAttributes: TStyledButtonAttributes;
  LDrawRect, LTextRect: TRect;
  LTextFlags: Cardinal;
  LStyle: TCustomStyleServices;
  LDetails: TThemedElementDetails;
  LColor: TColor;
  LThemeAttribute: TPanelThemeAttribute;
begin
  if not FStyleApplied then
    FStyleApplied := ApplyPanelStyle;

  LStyle := StyleServices{$IFDEF D10_4+}(Self){$ENDIF};
  LDetails := LStyle.GetElementDetails(tpPanelDontCare);

  //Get attributes based on Enabled state
  LAttributes := GetAttributes(Enabled);

  if not Assigned(LAttributes) then
  begin
    inherited;
    Exit;
  end;

  LDrawRect := ClientRect;

  //Setup Canvas Pen for Border
  if AsVCLStyle then
  begin
    //When AsVCLComponent is True, use BorderColor from TPanelThemeAttribute
    if GetPanelStyleAttributes(FStyleClass, LThemeAttribute) then
      Canvas.Pen.Color := LThemeAttribute.BorderColor
    else
      Canvas.Pen.Color := LAttributes.BorderColor;
  end
  else
    Canvas.Pen.Color := LAttributes.BorderColor;
  Canvas.Pen.Width := LAttributes.BorderWidth;
  Canvas.Pen.Style := LAttributes.PenStyle;

  //Setup Canvas Brush for Background
  if AsVCLStyle then
  begin
    //When AsVCLComponent is True, use PanelColor from TPanelThemeAttribute
    if GetPanelStyleAttributes(FStyleClass, LThemeAttribute) then
      Canvas.Brush.Color := LThemeAttribute.PanelColor
    else
      Canvas.Brush.Color := LAttributes.ButtonColor;
  end
  else
    Canvas.Brush.Color := LAttributes.ButtonColor;
  Canvas.Brush.Style := LAttributes.BrushStyle;

  //Setup Canvas Font
  Canvas.Font.Assign(Font);

  if AsVCLStyle then
  begin
    //When AsVCLComponent is True, use FontColor from ThemeAttribute
    if GetPanelStyleAttributes(FStyleClass, LThemeAttribute) then
      Canvas.Font.Color := LThemeAttribute.FontColor
    else
      Canvas.Font.Color := LAttributes.FontColor;
  end
  else if not (IsCustomStyleActive and not (seFont in StyleElements)) and
     LStyle.GetElementColor(LDetails, ecTextColor, LColor) and (LColor <> clNone) then
    Canvas.Font.Color := LColor
  else
    Canvas.Font.Color := LAttributes.FontColor;
  Canvas.Font.Style := LAttributes.FontStyle;

  //Draw Background and Border using CanvasDrawShape
  CanvasDrawShape(Canvas, LDrawRect,
    FStyleDrawType,
    FStyleRadius,
    FStyleRoundedCorners);

  //Draw Caption if ShowCaption and Caption is not empty
  if ShowCaption and (Caption <> '') then
  begin
    LTextRect := LDrawRect;

    //Build text flags based on alignment
    case FCaptionAlignment of
      taLeftJustify: LTextFlags := DT_LEFT or DT_VCENTER or DT_SINGLELINE;
      taRightJustify: LTextFlags := DT_RIGHT or DT_VCENTER or DT_SINGLELINE;
    else
      LTextFlags := DT_CENTER or DT_VCENTER or DT_SINGLELINE;
    end;

    //Apply BiDiMode flags
    LTextFlags := DrawTextBiDiModeFlags(LTextFlags);

    //Use DrawButtonText to render caption
    DrawButtonText(Canvas,
      Caption,
      FCaptionAlignment,
      FCaptionMargin,
      LAttributes.BorderWidth,
      LTextRect,
      LTextFlags,
      True); //PreserveBorders
  end;
end;

procedure TStyledPanel.SetCaptionAlignment(const AValue: TAlignment);
begin
  if FCaptionAlignment <> AValue then
  begin
    FCaptionAlignment := AValue;
    Invalidate;
  end;
end;

procedure TStyledPanel.SetCaptionMargin(const AValue: Integer);
begin
  if FCaptionMargin <> AValue then
  begin
    FCaptionMargin := AValue;
    Invalidate;
  end;
end;

function TStyledPanel.GetColor: TColor;
begin
  Result := Inherited Color;
end;

procedure TStyledPanel.SetColor(const AValue: TColor);
begin
  if inherited Color <> AValue then
  begin
    FPanelStyleNormal.ButtonColor := AValue;
    FCustomDrawType := True;
    Inherited Color := AValue;
    Invalidate;
  end;
end;

procedure TStyledPanel.SetCustomStyleDrawType(ACustomStyleDrawType: Boolean);
begin
  FCustomDrawType := ACustomStyleDrawType;
end;

procedure TStyledPanel.SetStyleRadius(const AValue: Integer);
begin
  if FStyleRadius <> AValue then
  begin
    FStyleRadius := AValue;
    Invalidate;
  end;
end;

procedure TStyledPanel.SetStyleRoundedCorners(const AValue: TRoundedCorners);
begin
  if FStyleRoundedCorners <> AValue then
  begin
    FStyleRoundedCorners := AValue;
    Invalidate;
  end;
end;

procedure TStyledPanel.SetStyleDrawType(const AValue: TStyledButtonDrawType);
begin
  if (FStyleDrawType <> AValue) or (csLoading in ComponentState) then
  begin
    FStyleDrawType := AValue;
    FCustomDrawType := True;
    ParentBackground := not IsStoredParentBackground;
    Invalidate;
  end;
end;

procedure TStyledPanel.SetStyleFamily(const AValue: TStyledButtonFamily);
var
  LValue: TStyledButtonFamily;
begin
  LValue := AValue;
  if LValue = '' then
    LValue := DEFAULT_CLASSIC_FAMILY;
  //Reject an unregistered family loudly, so a missing style unit surfaces at
  //design time instead of rendering black at runtime.
  if not StyleFamilyExists(LValue) then
    raise EStyledAttributesException.CreateFmt(ERROR_FAMILY_NOT_FOUND, [LValue]);
  if (LValue <> Self.FStyleFamily) or not FStyleApplied then
  begin
    FStyleFamily := LValue;
    FStyleApplied := ApplyPanelStyle;
  end;
  if FStyleFamily = DEFAULT_CLASSIC_FAMILY then
    StyleElements := [seFont, seClient, seBorder];
end;

procedure TStyledPanel.SetStyleClass(const AValue: TStyledButtonClass);
var
  LValue: TStyledButtonClass;
begin
  LValue := AValue;
  //Using a specific Class in Classic Family force
  //StyleElements without seClient
  if (FStyleFamily = DEFAULT_CLASSIC_FAMILY) then
  begin
    if (LValue <> DEFAULT_WINDOWS_CLASS) then
      StyleElements := StyleElements - [seClient];
    if LValue = '' then
      LValue := DEFAULT_WINDOWS_CLASS;
  end;
  if (LValue <> Self.FStyleClass) or not FStyleApplied then
  begin
    Self.FStyleClass := LValue;
    FStyleApplied := ApplyPanelStyle;
  end;
end;

procedure TStyledPanel.SetStyleAppearance(const AValue: TStyledButtonAppearance);
var
  LValue: TStyledButtonAppearance;
begin
  LValue := AValue;
  if LValue = '' then
    LValue := DEFAULT_APPEARANCE;
  if (LValue <> Self.FStyleAppearance) or not FStyleApplied then
  begin
    Self.FStyleAppearance := LValue;
    FStyleApplied := ApplyPanelStyle;
  end;
end;

procedure TStyledPanel.SetPanelStyle(const AStyleFamily: TStyledButtonFamily;
  const AStyleClass: TStyledButtonClass;
  const AStyleAppearance: TStyledButtonAppearance);
begin
  FStyleFamily := AStyleFamily;
  FStyleClass := AStyleClass;
  FStyleAppearance := AStyleAppearance;
  if not ApplyPanelStyle then
    raise EStyledPanelError.CreateFmt(ERROR_SETTING_PANEL_STYLE,
      [AStyleFamily, AStyleClass, AStyleAppearance]);
end;

procedure TStyledPanel.SetPanelStyleNormal(
  const AValue: TStyledButtonAttributes);
begin
  //Copy into the owned sub-object instead of swapping the pointer: a swap would
  //alias (and later double-free) another panel's attributes. Guard nil so
  //PanelStyleNormal := nil is a safe no-op instead of an AV in the comparison.
  if Assigned(AValue) and not SameStyledButtonStyle(FPanelStyleNormal, AValue) then
  begin
    FPanelStyleNormal.Assign(AValue);
    Invalidate;
  end;
end;

procedure TStyledPanel.SetPanelStyleDisabled(
  const AValue: TStyledButtonAttributes);
begin
  if Assigned(AValue) and not SameStyledButtonStyle(FPanelStyleDisabled, AValue) then
  begin
    FPanelStyleDisabled.Assign(AValue);
    Invalidate;
  end;
end;

function TStyledPanel.IsCustomDrawType: Boolean;
begin
  Result := FCustomDrawType;
end;

function TStyledPanel.IsCustomRoundedCorners: Boolean;
begin
  Result := FStyleRoundedCorners <> ALL_ROUNDED_CORNERS;
end;

function TStyledPanel.IsCustomRadius: Boolean;
begin
  Result := FStyleRadius <> _DefaultStyleRadius;
end;

function TStyledPanel.IsStoredStyleFamily: Boolean;
begin
  Result := FStyleFamily <> _DefaultFamily;
end;

function TStyledPanel.IsStoredStyleClass: Boolean;
begin
  Result := FStyleClass <> _DefaultClass;
end;

function TStyledPanel.IsStoredStyleElements: Boolean;
begin
  if FStyleFamily = DEFAULT_CLASSIC_FAMILY then
    Result := StyleElements <> [seFont, seClient, seBorder]
  else
    Result := False;
end;

function TStyledPanel.IsStoredColor: Boolean;
begin
  Result := inherited Color <> FPanelStyleNormal.ButtonColor;
end;

function TStyledPanel.IsStoredParentBackground: Boolean;
begin
  Result := not (FStyleDrawType in [btRoundRect, btRounded, btEllipse]);
end;

function TStyledPanel.IsStoredStyleAppearance: Boolean;
begin
  Result := FStyleAppearance <> _DefaultAppearance;
end;

class procedure TStyledPanel.RegisterDefaultRenderingStyle(
  const ADrawType: TStyledButtonDrawType;
  const AFamily: TStyledButtonFamily;
  const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance;
  const AStyleRadius: Integer);
begin
  _DefaultStyleDrawType := ADrawType;
  _DefaultFamily := AFamily;
  _DefaultClass := AClass;
  _DefaultAppearance := AAppearance;
  _DefaultStyleRadius := AStyleRadius;
end;

function TStyledPanel.GetActiveStyleName: string;
begin
  Result := Vcl.ButtonStylesAttributes.GetActiveStyleName(Self);
end;

function TStyledPanel.AsVCLStyle: Boolean;
begin
  Result := (FStyleFamily = DEFAULT_CLASSIC_FAMILY) and
    (seClient in StyleElements);
end;

function TStyledPanel.GetAsVCLComponent: Boolean;
begin
  Result := AsVCLStyle;
end;

procedure TStyledPanel.SetAsVCLComponent(const AValue: Boolean);
begin
  if AValue <> GetAsVCLComponent then
  begin
    if FStyleFamily <> DEFAULT_CLASSIC_FAMILY then
    begin
      FStyleFamily := DEFAULT_CLASSIC_FAMILY;
      FStyleClass := DEFAULT_WINDOWS_CLASS;
      FStyleAppearance := DEFAULT_APPEARANCE;
    end;
    if AValue then
      StyleElements := StyleElements + [seClient]
    else
      StyleElements := StyleElements - [seClient];
    FCustomDrawType := False;
    ApplyPanelStyle;
  end;
end;

initialization
  TStyledPanel._DefaultStyleDrawType := DEFAULT_STYLEDRAWTYPE;
  TStyledPanel._DefaultFamily := DEFAULT_CLASSIC_FAMILY;
  TStyledPanel._DefaultClass := DEFAULT_WINDOWS_CLASS;
  TStyledPanel._DefaultAppearance := DEFAULT_APPEARANCE;
  TStyledPanel._DefaultStyleRadius := DEFAULT_RADIUS;

end.
