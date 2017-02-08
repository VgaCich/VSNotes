unit GDIPAPI;

interface

uses Windows, AvL;

type
  TStatus = (Ok, GenericError, InvalidParameter, OutOfMemory, ObjectBusy,
    InsufficientBuffer, NotImplemented, Win32Error, WrongState, Aborted,
    FileNotFound, ValueOverflow, AccessDenied, UnknownImageFormat,
    FontFamilyNotFound, FontStyleNotFound, NotTrueTypeFont,
    UnsupportedGdiplusVersion, GdiplusNotInitialized, PropertyNotFound,
    PropertyNotSupported);
  TGdiplusStartupInput = packed record
    GdiplusVersion          : Cardinal;
    DebugEventCallback      : Pointer;
    SuppressBackgroundThread: BOOL;
    SuppressExternalCodecs  : BOOL;
  end;
  PGdiplusStartupInput = ^TGdiplusStartupInput;
  PGdiplusStartupOutput = Pointer;
  TImageCodecInfo = packed record
    Clsid             : TGUID;
    FormatID          : TGUID;
    CodecName         : PWCHAR;
    DllName           : PWCHAR;
    FormatDescription : PWCHAR;
    FilenameExtension : PWCHAR;
    MimeType          : PWCHAR;
    Flags             : DWORD;
    Version           : DWORD;
    SigCount          : DWORD;
    SigSize           : DWORD;
    SigPattern        : PBYTE;
    SigMask           : PBYTE;
  end;
  PImageCodecInfo = ^TImageCodecInfo;
  TImageCodecsInfo = array [0..MaxInt div SizeOf(TImageCodecInfo) - 1] of TImageCodecInfo;
  PImageCodecsInfo = ^TImageCodecsInfo;
  TEncoderParameter = packed record
    Guid           : TGUID;
    NumberOfValues : ULONG;
    Type_          : ULONG;
    Value          : Pointer;
  end;
  PEncoderParameter = ^TEncoderParameter;
  TEncoderParameters = packed record
    Count     : UINT;
    Parameter : array[0..0] of TEncoderParameter;
  end;
  PEncoderParameters = ^TEncoderParameters;
  TRotateFlip = (
    RotateNoneFlipNone = 0,
    Rotate90FlipNone   = 1,
    Rotate180FlipNone  = 2,
    Rotate270FlipNone  = 3,
    RotateNoneFlipX    = 4,
    Rotate90FlipX      = 5,
    Rotate180FlipX     = 6,
    Rotate270FlipX     = 7,
    RotateNoneFlipY    = Rotate180FlipX,
    Rotate90FlipY      = Rotate270FlipX,
    Rotate180FlipY     = RotateNoneFlipX,
    Rotate270FlipY     = Rotate90FlipX,
    RotateNoneFlipXY   = Rotate180FlipNone,
    Rotate90FlipXY     = Rotate270FlipNone,
    Rotate180FlipXY    = RotateNoneFlipNone,
    Rotate270FlipXY    = Rotate90FlipNone
  );
  TColorPalette256 = packed record
    Flags, Count: UINT;
    Entries: array[0..255] of DWORD;
  end;
  PColorPalette = ^TColorPalette256;
  TGPRect = packed record
    X     : Integer;
    Y     : Integer;
    Width : Integer;
    Height: Integer;
  end;
  PGPRect = ^TGPRect;
  TBitmapData = packed record
    Width       : UINT;
    Height      : UINT;
    Stride      : Integer;
    PixelFormat : Integer;
    Scan0       : Pointer;
    Reserved    : UINT;
  end;
  PBitmapData = ^TBitmapData;

const
  WINGDIPDLL = 'gdiplus.dll';
  EncoderParameterValueTypeByte          = 1;
  EncoderParameterValueTypeASCII         = 2;
  EncoderParameterValueTypeShort         = 3;
  EncoderParameterValueTypeLong          = 4;
  EncoderParameterValueTypeRational      = 5;
  EncoderParameterValueTypeLongRange     = 6;
  EncoderParameterValueTypeUndefined     = 7;
  EncoderParameterValueTypeRationalRange = 8;
  PaletteFlagsHasAlpha    = $0001;
  PaletteFlagsGrayScale   = $0002;
  PaletteFlagsHalftone    = $0004;
  ImageLockModeRead           = $0001;
  ImageLockModeWrite          = $0002;
  ImageLockModeUserInputBuf   = $0004;
  PixelFormatIndexed        = $00010000;
  PixelFormatGDI            = $00020000;
  PixelFormatAlpha          = $00040000;
  PixelFormatPAlpha         = $00080000;
  PixelFormatExtended       = $00100000;
  PixelFormatCanonical      = $00200000;
  PixelFormatUndefined      = 0;
  PixelFormatDontCare       = 0;
  PixelFormat1bppIndexed    = (1  or ( 1 shl 8) or PixelFormatIndexed or PixelFormatGDI);
  PixelFormat4bppIndexed    = (2  or ( 4 shl 8) or PixelFormatIndexed or PixelFormatGDI);
  PixelFormat8bppIndexed    = (3  or ( 8 shl 8) or PixelFormatIndexed or PixelFormatGDI);
  PixelFormat16bppGrayScale = (4  or (16 shl 8) or PixelFormatExtended);
  PixelFormat16bppRGB555    = (5  or (16 shl 8) or PixelFormatGDI);
  PixelFormat16bppRGB565    = (6  or (16 shl 8) or PixelFormatGDI);
  PixelFormat16bppARGB1555  = (7  or (16 shl 8) or PixelFormatAlpha or PixelFormatGDI);
  PixelFormat24bppRGB       = (8  or (24 shl 8) or PixelFormatGDI);
  PixelFormat32bppRGB       = (9  or (32 shl 8) or PixelFormatGDI);
  PixelFormat32bppARGB      = (10 or (32 shl 8) or PixelFormatAlpha or PixelFormatGDI or PixelFormatCanonical);
  PixelFormat32bppPARGB     = (11 or (32 shl 8) or PixelFormatAlpha or PixelFormatPAlpha or PixelFormatGDI);
  PixelFormat48bppRGB       = (12 or (48 shl 8) or PixelFormatExtended);
  PixelFormat64bppARGB      = (13 or (64 shl 8) or PixelFormatAlpha  or PixelFormatCanonical or PixelFormatExtended);
  PixelFormat64bppPARGB     = (14 or (64 shl 8) or PixelFormatAlpha  or PixelFormatPAlpha or PixelFormatExtended);
  PixelFormatMax            = 15;
  EncoderQuality: TGUID = '{1d5be4b5-fa4a-452d-9cdd-5db35105e7eb}';

function GdiplusStartup(out token: ULONG; input: PGdiplusStartupInput; output: PGdiplusStartupOutput): TStatus; stdcall; external WINGDIPDLL name 'GdiplusStartup';
procedure GdiplusShutdown(token: ULONG); stdcall; external WINGDIPDLL name 'GdiplusShutdown';
function GdipGetImageEncodersSize(out numEncoders: UINT; out size: UINT): TStatus; stdcall; external WINGDIPDLL name 'GdipGetImageEncodersSize';
function GdipGetImageEncoders(numEncoders: UINT; size: UINT; encoders: PImageCodecInfo): TStatus; stdcall; external WINGDIPDLL name 'GdipGetImageEncoders';
function GdipCreateBitmapFromScan0(width: Integer; height: Integer; stride: Integer; format: Integer; scan0: Pointer; out bitmap: Pointer): TStatus; stdcall; external WINGDIPDLL name 'GdipCreateBitmapFromScan0';
function GdipCreateBitmapFromFile(filename: PWCHAR; out bitmap: Pointer): TStatus; stdcall; external WINGDIPDLL name 'GdipCreateBitmapFromFile';
function GdipCreateBitmapFromStream(stream: IUnknown; out bitmap: Pointer): TStatus; stdcall; external WINGDIPDLL name 'GdipCreateBitmapFromStream';
function GdipSaveImageToFile(image: Pointer; filename: PWChar; clsidEncoder: PGUID; encoderParams: PEncoderParameters): TStatus; stdcall; external WINGDIPDLL name 'GdipSaveImageToFile';
function GdipSaveImageToStream(image: Pointer; stream: IUnknown; clsidEncoder: PGUID; encoderParams: PEncoderParameters): TStatus; stdcall; external WINGDIPDLL name 'GdipSaveImageToStream';
function GdipDisposeImage(image: Pointer): TStatus; stdcall; external WINGDIPDLL name 'GdipDisposeImage';
function GdipGetImagePalette(image: Pointer; out palette: TColorPalette256; size: Integer): TStatus; stdcall;  external WINGDIPDLL name 'GdipGetImagePalette';
function GdipSetImagePalette(image: Pointer; const palette: TColorPalette256): TStatus; stdcall; external WINGDIPDLL name 'GdipSetImagePalette';
function GdipGetImagePaletteSize(image: Pointer; var size: Integer): TStatus; stdcall; external WINGDIPDLL name 'GdipGetImagePaletteSize';
function GdipImageRotateFlip(image: Pointer; rfType: TRotateFlip): TStatus; stdcall; external WINGDIPDLL name 'GdipImageRotateFlip';
function GdipBitmapLockBits(bitmap: Pointer; const rect: TGPRect; flags: UINT; format: Integer; lockedBitmapData: PBitmapData): TStatus; stdcall; external WINGDIPDLL name 'GdipBitmapLockBits';
function GdipBitmapUnlockBits(bitmap: Pointer; lockedBitmapData: PBitmapData): TStatus; stdcall; external WINGDIPDLL name 'GdipBitmapUnlockBits';
function GdipGetImageWidth(image: Pointer; var width: UINT): TStatus; stdcall; external WINGDIPDLL name 'GdipGetImageWidth';
function GdipGetImageHeight(image: Pointer; var height: UINT): TStatus; stdcall; external WINGDIPDLL name 'GdipGetImageHeight';
function GdipGetImagePixelFormat(image: Pointer; out format: Integer): TStatus; stdcall; external WINGDIPDLL name 'GdipGetImagePixelFormat';
function GdipCreateHBITMAPFromBitmap(bitmap: Pointer; var hbmReturn: hBitmap; background: TColor): TStatus; stdcall; external WINGDIPDLL name 'GdipCreateHBITMAPFromBitmap';

var
  GdiplusInitStatus: TStatus = GdiplusNotInitialized;

implementation

var
  GdiplusStartupInput: TGdiplusStartupInput = (GdiplusVersion: 1; DebugEventCallback: nil; SuppressBackgroundThread: false; SuppressExternalCodecs: false);
  GdiplusToken: ULONG = 0;

initialization
  GdiplusInitStatus := GdiplusStartup(GdiplusToken, @GdiplusStartupInput, nil);

finalization
  GdiplusShutdown(GdiplusToken);

end.