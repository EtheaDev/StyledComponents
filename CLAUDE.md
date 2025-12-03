# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Delphi VCL StyledComponents is a library of custom VCL components that provide styled buttons, panels, toolbars, navigators, button groups, and dialogs with modern appearance beyond standard Windows theming. Supports Delphi XE6 through Delphi 13 (32/64-bit).

Current version: 3.9.1

## Build and Package Structure

### Package Organization

The project uses separate package group files for each Delphi version:

- **Core packages**: `packages/D{version}/1.Vcl.StyledComponents.groupproj`
  - StyledComponents.dpk (runtime)
  - dclStyledComponents.dpk (design-time)

- **Animated packages** (optional, requires Skia4Delphi): `packages/D{version}/2.Vcl.StyledAnimatedComponents.groupproj`
  - StyledAnimatedComponents.dpk (runtime)
  - dclStyledAnimatedComponents.dpk (design-time)

Supported Delphi versions: DXE6, DXE7, DXE8, D10, D10_1, D10_2, D10_3, D10_4, D11, D12, D13

### Building Packages

Open the appropriate group project file for your Delphi version and build both packages (runtime first, then design-time). The installer (Setup/InnoSetupScripts) automates this for releases.

## Core Architecture

### The Rendering System: TStyledButtonRender

The entire library is built around **TStyledButtonRender** - a single non-visual class that handles all button rendering logic. Every styled component uses composition with this class rather than inheritance, eliminating code duplication across 40+ component variants.

**Key insight**: All visual appearance (shapes, colors, states) is delegated to TStyledButtonRender. Components are thin wrappers that expose properties and handle user interaction.

### Three-Tier Styling System

The styling system has three levels that combine to determine appearance:

1. **StyleFamily** - Global appearance theme:
   - Classic, Bootstrap, Angular-Light, Angular-Dark, Basic-Colors, SVG-Colors
   - Implemented via IStyledButtonAttributes interface providers
   - Each family lives in its own unit (Vcl.StandardButtonStyles, Vcl.BootstrapButtonStyles, etc.)

2. **StyleClass** - Predefined color/style within a family:
   - Bootstrap: Primary, Secondary, Success, Danger, Warning, Info, Light, Dark
   - Angular: Basic, Warn, Link, DeepPurple, Amber, Indigo, Pink
   - Color families: Actual color names

3. **StyleAppearance** - Visual variant:
   - Normal, Outline, Flat, Raised, Basic, Stroked (varies by family)

Each combination maps to 5 state-specific `TStyledButtonAttributes` objects (Normal, Pressed, Selected, Hot, Disabled) defining button color, border color, and font color.

### Component Hierarchy

**Base Components** (use TStyledButtonRender directly):
- TStyledGraphicButton - Pure graphic button (no focus/tabstop)
- TStyledButton - Windowed button with focus support
- TStyledSpeedButton - TSpeedButton compatibility (with Layout/Margin/Spacing)
- TStyledBitBtn - TBitBtn compatibility (with Glyph/NumGlyphs)
- TStyledAnimatedButton - Skia4Delphi animated button (separate package)

**Container Components** (wrap multiple TStyledButtonRender instances):
- TStyledToolbar - Extends TFlowPanel, contains TStyledToolButtons
- TStyledDbNavigator - Database navigator with styled buttons
- TStyledBindNavigator - LiveBindings navigator with styled buttons
- TStyledButtonGroup - Extends TButtonGroup
- TStyledCategoryButtons - Extends TCategoryButtons

**Panel Component** (uses TStyledButtonAttributes directly):
- TStyledPanel - Extends TCustomPanel with styled background rendering
  - Uses TStyledButtonAttributes for Normal and Disabled states (no TStyledButtonRender)
  - Supports all styling properties: StyleFamily, StyleClass, StyleAppearance, StyleDrawType, StyleRadius, StyleRoundedCorners
  - Caption rendering with CaptionAlignment and CaptionMargin properties
  - Can switch between styled and VCL rendering via AsVCLComponent property
  - Fully compatible with standard TPanel/TCustomPanel properties (Bevel, Border, Alignment, etc.)

**Dialog System**:
- TStyledTaskDialog - Extends TTaskDialog with styled buttons and animations
- Vcl.StyledTaskDialogFormUnit - Form base class for dialogs
- Skia.Vcl.StyledTaskDialogAnimatedUnit - Animated dialogs with Lottie support

### Key Source Files

- `source/Vcl.ButtonStylesAttributes.pas` - Core attributes and TStyledButtonRender class
- `source/Vcl.StyledButton.pas` - All button components
- `source/Vcl.Standard*.pas`, `Vcl.Bootstrap*.pas`, `Vcl.Angular*.pas` - Style families
- `source/Vcl.StyledToolbar.pas` - Toolbar component
- `source/Vcl.StyledDbNavigator.pas` - Navigator components
- `source/Vcl.StyledButtonGroup.pas` / `Vcl.StyledCategoryButtons.pas` - Group components
- `source/Vcl.StyledPanel.pas` - Panel component with styled background
- `source/Vcl.StyledTaskDialog.pas` - Dialog component
- `source/Vcl.TemplateButtonStyles.pas` - Template for creating custom style families

## Development Patterns

### Creating Custom Style Families

Use `Vcl.TemplateButtonStyles.pas` as a template. Key steps:

1. Implement IStyledButtonAttributes interface
2. Define family name constant and class name constants
3. Implement UpdateAttributes() to set colors for each class/appearance/state combination
4. Register in unit initialization section
5. Add unit to StyledComponents package

### Extending Components

When creating new styled components:

1. Inherit from appropriate VCL base (TGraphicControl or TWinControl)
2. Create FRender: TStyledButtonRender instance in constructor
3. Delegate Paint/DrawButton to FRender.DrawButton()
4. Expose styling properties (StyleFamily, StyleClass, StyleAppearance, etc.)
5. Handle state changes (mouse events, focus, enabled) and call FRender.Invalidate

### Creating Styled Panels

TStyledPanel uses a different pattern from buttons:

1. Inherits from TCustomPanel (container control)
2. Uses TStyledButtonAttributes directly (no TStyledButtonRender instance)
3. Maintains two attribute instances: FPanelStyleNormal and FPanelStyleDisabled
4. Custom Paint() method renders background using GDI+ based on current Enabled state
5. Supports switching between styled and VCL rendering via AsVCLComponent property
6. Register default styles with TStyledPanel.RegisterDefaultRenderingStyle()

### Modal Result Integration

Buttons automatically style based on ModalResult:
- mrOk, mrYes → Success/Primary colors
- mrNo, mrAbort → Danger/Error colors
- mrCancel → Warning colors
- mrHelp → Info colors

Override via IStyledButtonAttributes.GetStyleByModalResult() in custom families.

### DPI Scaling

All rendering respects DPI scaling automatically via TStyledButtonRender.GetOwnerScaleFactor(). Border widths, margins, icon sizes, and font sizes scale proportionally.

## Working with Demos

The main demo is **Demos/Delphi{version}/StyledComponentsDemo.dproj** which shows all components in action via embedded forms:

- Forms load into MainDemoForm client area
- Each form demonstrates a specific component or feature
- Source files shared across versions in `Demos/source/`

**Running with Skia animations**:
- Install Skia4Delphi first
- Build StyledAnimatedComponents packages
- Enable SKIA in project options (context menu → "Enable SKIA")
- AnimatedButtonsForm and animated dialogs will work

## Replacing Standard VCL Components

### Global Replacement via Interposer

Add `Vcl.StyledComponentsHooks.pas` to your project to replace standard buttons globally via unit aliasing.

### Message Dialog Replacement

To replace MessageDlg/TaskDialog with styled versions:

1. Add `Vcl.StyledTaskDialog` to uses clause
2. Replace calls:
  > `MessageDlg` → `StyledMessageDlg`

  > `TaskDialog` → `StyledTaskDialog`

3. For animated dialogs, add `Skia.Vcl.StyledTaskDialogAnimatedUnit`

4. Configure defaults:

```pascal
uses
  Vcl.StyledButton, Vcl.ButtonStylesAttributes, Vcl.StyledTaskDialog;

begin
  Application.Initialize;

  // Set default button styles
  TStyledButton.RegisterDefaultRenderingStyle(btRounded, BOOTSTRAP_FAMILY, btn_primary, BOOTSTRAP_NORMAL);

  // Configure dialog fonts and button family
  InitializeStyledTaskDialogs(True, Screen.MessageFont, BOOTSTRAP_FAMILY);
end;
```

## Important Technical Details

### GDI+ Rendering

Components use GDI+ for antialiased rendering, NOT VCL Styles. This means:
- Styled buttons look identical with or without VCL Styles applied
- Shapes (rounded, circular) render smoothly with antialiasing
- Requires Windows XP SP2+ (available on all supported Delphi target platforms)

### Lazy Style Resolution

Styles are computed on-demand when StyleFamily/StyleClass/StyleAppearance properties change. The FStyleApplied flag tracks whether attributes need recalculation. This enables fast designer manipulation without forcing immediate resolution.

### Notification Badge System

TNotificationBadgeAttributes provides badge rendering (counter/dot) on any styled button. Badges appear in top-right corner with customizable:
- Text/count
- Background color
- Shape (circle, rounded rect)
- Visibility

Supported on all button types including toolbar buttons and group items.

### AutoClick Feature

All styled buttons support AutoClick/AutoClickDelay to trigger Click event after a delay. Useful for:
- Automated dialogs (countdown before auto-close)
- Timed confirmations
- Demo/tutorial modes

The StyledTaskDialog component also supports this for auto-closing dialogs.

## Setup/Installation

The installer source is in `Setup/InnoSetupScripts/`.

- Detects installed Delphi versions
- Copies sources to {User}\Documents\Embarcadero\Studio\StyledComponents
- Builds packages for each detected version
- Installs packages into IDE
- Adds source paths to library path


