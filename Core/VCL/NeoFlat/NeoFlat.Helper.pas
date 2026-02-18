{******************************************************************************}
{                                                                              }
{         ____             _     ____          _           ____                }
{        |  _ \  __ _ _ __| | __/ ___|___   __| | ___ _ __/ ___|  ___          }
{        | | | |/ _` | '__| |/ / |   / _ \ / _` |/ _ \ '__\___ \ / __|         }
{        | |_| | (_| | |  |   <| |__| (_) | (_| |  __/ |   ___) | (__          }
{        |____/ \__,_|_|  |_|\_\\____\___/ \__,_|\___|_|  |____/ \___|         }
{                             Project: Optix Gate                              }
{                                                                              }
{                                                                              }
{                   Author: DarkCoderSc (Jean-Pierre LESUEUR)                  }
{                   https://www.twitter.com/darkcodersc                        }
{                   https://bsky.app/profile/darkcodersc.bsky.social           }
{                   https://github.com/darkcodersc                             }
{                   License: (!) CHECK README.md (!)                           }
{                                                                              }
{                                                                              }
{                                                                              }
{  Disclaimer:                                                                 }
{  -----------                                                                 }
{    We are doing our best to prepare the content of this app and/or code.     }
{    However, The author cannot warranty the expressions and suggestions       }
{    of the contents, as well as its accuracy. In addition, to the extent      }
{    permitted by the law, author shall not be responsible for any losses      }
{    and/or damages due to the usage of the information on our app and/or      }
{    code.                                                                     }
{                                                                              }
{    By using our app and/or code, you hereby consent to our disclaimer        }
{    and agree to its terms.                                                   }
{                                                                              }
{    Any links contained in our app may lead to external sites are provided    }
{    for convenience only.                                                     }
{    Any information or statements that appeared in these sites or app or      }
{    files are not sponsored, endorsed, or otherwise approved by the author.   }
{    For these external sites, the author cannot be held liable for the        }
{    availability of, or the content located on or through it.                 }
{    Plus, any losses or damages occurred from using these contents or the     }
{    internet generally.                                                       }
{                                                                              }
{******************************************************************************}

unit NeoFlat.Helper;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes,

  WinAPI.Windows,

  VCL.Graphics, VCL.ImgList, VCL.Controls,

  NeoFlat.Types;
// ---------------------------------------------------------------------------------------------------------------------

type
  TFlatMetrics = class
  private
    FControl : TControl;

    {@M}
    function GetMetric(const AIndex : Integer) : Integer;
    function GetScaleFactor() : Single;
  public
    {@C}
    constructor Create(const AControl : TControl);

    {@M}
    function ScaleValue(const AValue : Integer) : Integer;

    {@G}
    property _1  : Integer index 1  read GetMetric;
    property _2  : Integer index 2  read GetMetric;
    property _3  : Integer index 3  read GetMetric;
    property _4  : Integer index 4  read GetMetric;
    property _5  : Integer index 5  read GetMetric;
    property _6  : Integer index 6  read GetMetric;
    property _7  : Integer index 7  read GetMetric;
    property _8  : Integer index 8  read GetMetric;
    property _9  : Integer index 9  read GetMetric;
    property _10 : Integer index 10 read GetMetric;
    property _11 : Integer index 11 read GetMetric;
    property _12 : Integer index 12 read GetMetric;
    property _13 : Integer index 13 read GetMetric;
    property _14 : Integer index 14 read GetMetric;
    property _15 : Integer index 15 read GetMetric;
    property _16 : Integer index 16 read GetMetric;
    property _17 : Integer index 17 read GetMetric;
    property _18 : Integer index 18 read GetMetric;
    property _19 : Integer index 19 read GetMetric;
    property _20 : Integer index 20 read GetMetric;

    property ScaleFactor : Single read GetScaleFactor;
  end;

procedure FadeBitmap32Opacity(var ABitmap : TBitmap; const AOpacity : Word = 200);
procedure InitializeBitmap32(var ABmp : TBitmap; AWidth, AHeight : Integer);
function GetParentChain(ABase : TComponent) : String;
procedure DrawGlyph(const ACanvas : TCanvas; const AImageList : TCustomImageList; const AImageIndex : Integer;
  const X, Y : Integer; const AEnabled : Boolean);
procedure ScaleMatrixGlyph(const AGlyph: TMatrixGlyph; var AScaledGlyph : TMatrixGlyph; const AScaleFactor : Integer);
function RotateMatrixGlyph(const AGlyph: TByteArrayArray; AClockwise : Boolean = true) : TMatrixGlyph;
procedure DrawMatrixGlyph(const ACanvas : TCanvas; const AGlyphMatrix : TMatrixGlyph; const X, Y : Integer;
  const AGlyphColor : TColor); overload;
procedure DrawMatrixGlyph(const ACanvas : TCanvas; const AGlyphMatrix : TMatrixGlyph; const APoint : TPoint;
  const AGlyphColor : TColor); overload;
function IsValidMatrixGlyph(const AGlyph : TMatrixGlyph) : Boolean;

implementation

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.SysUtils;
// ---------------------------------------------------------------------------------------------------------------------

(* Local *)

function IsValidMatrixGlyph(const AGlyph : TMatrixGlyph) : Boolean;
begin
  result := (Length(AGlyph) > 0) and (Length(AGlyph[0]) > 0);
end;

procedure DrawMatrixGlyph(const ACanvas : TCanvas; const AGlyphMatrix : TMatrixGlyph; const X, Y : Integer;
  const AGlyphColor : TColor);
begin
  if not Assigned(ACanvas) or not IsValidMatrixGlyph(AGlyphMatrix) then
    Exit;
  ///

  for var I := 0 to Length(AGlyphMatrix[0]) -1 do
    for var N := 0 to Length(AGlyphMatrix) -1 do
      if AGlyphMatrix[N][I] <> 0 then
        ACanvas.Pixels[(X + I), (Y + N)] := AGlyphColor;
end;

procedure DrawMatrixGlyph(const ACanvas : TCanvas; const AGlyphMatrix : TMatrixGlyph; const APoint : TPoint;
  const AGlyphColor : TColor);
begin
  DrawMatrixGlyph(ACanvas, AGlyphMatrix, APoint.X, APoint.Y, AGlyphColor);
end;

procedure ScaleMatrixGlyph(const AGlyph: TMatrixGlyph; var AScaledGlyph : TMatrixGlyph; const AScaleFactor : Integer);
begin
  if not IsValidMatrixGlyph(AGlyph) then begin
    SetLength(AScaledGlyph, 0);
    Exit;
  end;
  ///

  var AOriginalRows := Length(AGlyph);
  var AOriginalCols := Length(AGlyph[0]);

  var ANewRows := AOriginalRows * AScaleFactor;
  var ANewCols := AOriginalCols * AScaleFactor;

  SetLength(AScaledGlyph, ANewRows, ANewCols);

  for var i := 0 to AOriginalRows - 1 do
    for var j := 0 to AOriginalCols - 1 do
      for var k := 0 to AScaleFactor - 1 do
        for var l := 0 to AScaleFactor - 1 do
          AScaledGlyph[i * AScaleFactor + k][j * AScaleFactor + l] := AGlyph[i][j];
end;

function RotateMatrixGlyph(const AGlyph: TByteArrayArray; AClockwise : Boolean = true) : TMatrixGlyph;
begin
  result := nil;
  ///

  var ARowLength := Length(AGlyph);
  if ARowLength = 0 then
    Exit;

  var AColumnLength := Length(AGlyph[0]);

  SetLength(result, AColumnLength, ARowLength);

  for var X := 0 to ARowLength - 1 do begin
    for var Y := 0 to AColumnLength - 1 do begin
      if AClockwise then
        result[Y][(ARowLength - 1) - X] := AGlyph[X][Y]
      else
        result[(AColumnLength - 1) - Y][X] := AGlyph[X][Y];
    end;
  end;
end;

procedure DrawGlyph(const ACanvas : TCanvas; const AImageList : TCustomImageList; const AImageIndex : Integer;
  const X, Y : Integer; const AEnabled : Boolean);
begin
  if not Assigned(AImageList) or (AImageIndex <= -1) then
    Exit;

  var AGlyph := TBitmap.Create();
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

function GetParentChain(ABase : TComponent) : String;
begin
  result := '';
  ///

  if not Assigned(ABase) then
    Exit;

  var AList := TStringList.Create();
  try
    AList.Add(ABase.Name);

    while True do begin
      if ABase.Owner = nil then
        break;

      ABase := ABase.Owner;

      if ABase.Name <> '' then
        AList.Add(ABase.Name);
    end;

    for var I := AList.Count -1 downto 0 do begin
      if I <> AList.Count -1 then
        result := result + '.';

      result := result + AList.Strings[I];
    end;
  finally
    if Assigned(AList) then
      FreeAndNil(AList);
  end;
end;

procedure InitializeBitmap32(var ABmp : TBitmap; AWidth, AHeight : Integer);
begin
  if NOT Assigned(ABmp) then
    Exit;
  ///

  ABmp.PixelFormat   := pf32Bit;
  ABmp.Width         := AWidth;
  ABmp.Height        := AHeight;
  ABmp.HandleType    := bmDIB;
  ABmp.ignorepalette := true;
  ABmp.alphaformat   := afPremultiplied;

  var p := ABmp.ScanLine[AHeight - 1];
  ZeroMemory(p, AWidth * AHeight * 4);
end;

function ValidGraphic(ABitmap : TBitmap) : Boolean;
begin
  result := Assigned(ABitmap) and (ABitmap.Width > 0) and (ABitmap.Height > 0);
end;

procedure FadeBitmap32Opacity(var ABitmap : TBitmap; const AOpacity : Word = 200);
begin
  if not ValidGraphic(ABitmap) then
    Exit;
  ///

  for var y := 0 to ABitmap.Height-1 do begin
    var pPixels := PRGBQuad(ABitmap.ScanLine[y]);
    for var x := 0 to ABitmap.Width-1 do begin
      try
        if pPixels^.rgbReserved = 0 then continue;

        var APixelOpacity : Word := (pPixels^.rgbReserved * AOpacity) div high(byte);

        pPixels^.rgbRed   := (pPixels^.rgbRed   * APixelOpacity) div $FF;
        pPixels^.rgbGreen := (pPixels^.rgbGreen * APixelOpacity) div $FF;
        pPixels^.rgbBlue  := (pPixels^.rgbBlue  * APixelOpacity) div $FF;

        pPixels^.rgbReserved := APixelOpacity;
      finally
        Inc(pPixels);
      end;
    end;
  end;
end;

(* TFlatMetrics *)

function TFlatMetrics.GetMetric(const AIndex : Integer) : Integer;
begin
  if not Assigned(FControl) then
    result := AIndex
  else
    result := FControl.ScaleValue(AIndex);
end;

constructor TFlatMetrics.Create(const AControl : TControl);
begin
  inherited Create();
  ///

  FControl := AControl;
end;

function TFlatMetrics.GetScaleFactor() : Single;
begin
  if not Assigned(FControl) then
    result := 1
  else
    result := FControl.ScaleFactor;
end;

function TFlatMetrics.ScaleValue(const AValue : Integer) : Integer;
begin
  if not Assigned(FControl) then
    result := AValue
  else
    result := FControl.ScaleValue(AValue);
end;

end.
