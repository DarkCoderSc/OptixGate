{******************************************************************************}
{                                                                              }
{         ____             _     ____          _           ____                }
{        |  _ \  __ _ _ __| | __/ ___|___   __| | ___ _ __/ ___|  ___          }
{        | | | |/ _` | '__| |/ / |   / _ \ / _` |/ _ \ '__\___ \ / __|         }
{        | |_| | (_| | |  |   <| |__| (_) | (_| |  __/ |   ___) | (__          }
{        |____/ \__,_|_|  |_|\_\\____\___/ \__,_|\___|_|  |____/ \___|         }
{                              Project: Optix Neo                              }
{                                                                              }
{                                                                              }
{                   Author: DarkCoderSc (Jean-Pierre LESUEUR)                  }
{                   https://www.twitter.com/darkcodersc                        }
{                   https://github.com/darkcodersc                             }
{                   License: Apache License 2.0                                }
{                                                                              }
{                                                                              }
{    I dedicate this work to my daughter & wife                                }
{                                                                              }
{******************************************************************************}

unit NeoFlat.Common;

interface

uses WinAPI.Windows, VCL.Graphics, System.Classes, VCL.ImgList, NeoFlat.Types;

function FadeBitmap32Opacity(var ABitmap : TBitmap; AOpacity : word = 200) : Boolean;
procedure InitializeBitmap32(var ABmp : TBitmap; AWidth, AHeight : Integer);
function GetParentChain(ABase : TComponent) : String;
procedure DrawGlyph(const ACanvas : TCanvas; const AImageList : TCustomImageList; const AImageIndex : Integer; const X, Y : Integer; const AEnabled : Boolean);
procedure ScaleMatrixGlyph(const AGlyph: TByteArrayArray; var AScaledGlyph : TByteArrayArray; const AScaleFactor : Integer);
procedure DrawMatrixGlyph(const ACanvas : TCanvas; const AGlyphMatrix : TByteArrayArray; const X, Y : Integer; const AGlyphColor : TColor);

implementation

uses System.SysUtils;

procedure DrawMatrixGlyph(const ACanvas : TCanvas; const AGlyphMatrix : TByteArrayArray; const X, Y : Integer; const AGlyphColor : TColor);
var I, N         : Integer;
    AGlyphWidth  : Integer;
    AGlyphHeight : Integer;
begin
  if not Assigned(ACanvas) then
    Exit();
  ///

  AGlyphWidth  := High(AGlyphMatrix[1]) + 1;
  AGlyphHeight := High(AGlyphMatrix) + 1;
  ///

  for I := 0 to AGlyphWidth -1 do begin
    for N := 0 to AGlyphHeight -1 do begin
      if AGlyphMatrix[N][I] <> 0 then begin
        ACanvas.Pixels[(X + I), (Y + N)] := AGlyphColor;
      end;
    end;
  end;
end;

procedure ScaleMatrixGlyph(const AGlyph: TByteArrayArray; var AScaledGlyph : TByteArrayArray; const AScaleFactor : Integer);
var
  AOriginalRows : Integer;
  AOriginalCols : Integer;
  ANewRows      : Integer;
  ANewCols      : Integer;
  i, j, k, l    : Integer;
begin
  // Get the original size of the glyph
  AOriginalRows := Length(AGlyph);
  AOriginalCols := Length(AGlyph[0]);

  // Calculate the new size of the scaled glyph
  ANewRows := AOriginalRows * AScaleFactor;
  ANewCols := AOriginalCols * AScaleFactor;

  // Resize the scaled glyph array
  SetLength(AScaledGlyph, ANewRows, ANewCols);

  // Scale the glyph by repeating each pixel AScaleFactor times in both dimensions
  for i := 0 to AOriginalRows - 1 do
    for j := 0 to AOriginalCols - 1 do
      for k := 0 to AScaleFactor - 1 do
        for l := 0 to AScaleFactor - 1 do
          AScaledGlyph[i * AScaleFactor + k][j * AScaleFactor + l] := AGlyph[i][j];
end;

{ _.DrawGlyph }
procedure DrawGlyph(const ACanvas : TCanvas; const AImageList : TCustomImageList; const AImageIndex : Integer; const X, Y : Integer; const AEnabled : Boolean);
var AGlyph : TBitmap;
begin
  if not Assigned(AImageList) or (AImageIndex <= -1) then
    Exit();

  AGlyph := TBitmap.Create();
  try
    InitializeBitmap32(AGlyph, AImageList.Width, AImageList.Height);

    AImageList.GetBitmap(AImageIndex, AGlyph);

    if not AEnabled then
      FadeBitmap32Opacity(AGlyph, 100);

    ///
    ACanvas.Draw(X, Y, AGlyph);
  finally
    if Assigned(AGlyph) then
      FreeAndNil(AGlyph);
  end;
end;

{ _.GetParentChain

  TODO: Improve this code with recursion instead of using TStringList like a lazy
  guy }

function GetParentChain(ABase : TComponent) : String;
var AList : TStringList;
    I     : Integer;
begin
  result := '';

  if not Assigned(ABase) then
    Exit();

  AList := TStringList.Create();
  try
    AList.Add(ABase.Name);

    while True do begin
      if ABase.Owner = nil then
        break;

      ABase := ABase.Owner;

      if ABase.Name <> '' then
        AList.Add(ABase.Name);
    end;

    for I := AList.Count -1 downto 0 do begin
      if I <> AList.Count -1 then
        result := result + '.';

      result := result + AList.Strings[I];
    end;
  finally
    if Assigned(AList) then
      FreeAndNil(AList);
  end;
end;

{-------------------------------------------------------------------------------
  Initialize Transparent Bitmap
-------------------------------------------------------------------------------}
procedure InitializeBitmap32(var ABmp : TBitmap; AWidth, AHeight : Integer);
var p : pointer;
begin
  if NOT Assigned(ABmp) then
    Exit;

  ABmp.PixelFormat   := pf32Bit;
  ABmp.Width         := AWidth;
  ABmp.Height        := AHeight;
  ABmp.HandleType    := bmDIB;
  ABmp.ignorepalette := true;
  ABmp.alphaformat   := afPremultiplied;

  p := ABmp.ScanLine[AHeight - 1];
  ZeroMemory(p, AWidth * AHeight * 4);
end;

{-------------------------------------------------------------------------------
  Check if Bitmap is well formed
-------------------------------------------------------------------------------}
function ValidGraphic(ABitmap : TBitmap) : Boolean;
begin
  result := false;
  ///

  if NOT Assigned(ABitmap) then
    Exit;

  if ABitmap.Width  <= 0 then exit;
  if ABitmap.Height <= 0 then exit;

  ///
  result := true;
end;

{-------------------------------------------------------------------------------
  Fade 32Bit bitmap (Change opacity level)
-------------------------------------------------------------------------------}
function FadeBitmap32Opacity(var ABitmap : TBitmap; AOpacity : word = 200) : Boolean;
var x, y    : Integer;
    pPixels : PRGBQuad;

    APixelOpacity : word;
begin
  result := false;
  ///

  if NOT ValidGraphic(ABitmap) then exit;

  {
    START FADING BITMAP PIXEL
  }
  for y := 0 to ABitmap.Height-1 do begin
    pPixels := PRGBQuad(ABitmap.ScanLine[y]);

    {WORK ON THE LINE}
    for x := 0 to ABitmap.Width-1 do begin
      try
        if pPixels^.rgbReserved = 0 then continue;

        {
          WE NEED TO TAKE CARE OF CURRENT PIXEL TRANSPARENCY LEVEL TO BE SMOOTH
        }
        APixelOpacity := (pPixels^.rgbReserved * AOpacity) div high(byte);

        {
          UPDATE PIXEL COLOR USING NEW OPACITY LEVEL
        }
        pPixels^.rgbRed   := (pPixels^.rgbRed   * APixelOpacity) div $FF;
        pPixels^.rgbGreen := (pPixels^.rgbGreen * APixelOpacity) div $FF;
        pPixels^.rgbBlue  := (pPixels^.rgbBlue  * APixelOpacity) div $FF;

        {
          THEN SET THE OPACITY LEVEL OF THE CURRENT PIXEL
        }
        pPixels^.rgbReserved := APixelOpacity;
      finally
        Inc(pPixels);
      end;
    end;
  end;
end;

end.
