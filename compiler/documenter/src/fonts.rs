define! {
    copy_over;
    // "OpenSans-Light.ttf",
    // "OpenSans-LightItalic.ttf",
    "OpenSans-Regular.ttf",
    "OpenSans-Italic.ttf",
    // "OpenSans-Medium.ttf",
    // "OpenSans-MediumItalic.ttf",
    "OpenSans-SemiBold.ttf",
    "OpenSans-SemiBoldItalic.ttf",
    "OpenSans-Bold.ttf",
    "OpenSans-BoldItalic.ttf",
    // "OpenSans-ExtraBold.ttf",
    // "OpenSans-ExtraBoldItalic.ttf",
    // "SourceCodePro-ExtraLight.ttf",
    // "SourceCodePro-ExtraLightItalic.ttf",
    // "SourceCodePro-Light.ttf",
    // "SourceCodePro-LightItalic.ttf",
    "SourceCodePro-Regular.ttf",
    "SourceCodePro-Italic.ttf",
    // "SourceCodePro-Medium.ttf",
    // "SourceCodePro-MediumItalic.ttf",
    // "SourceCodePro-SemiBold.ttf",
    // "SourceCodePro-SemiBoldItalic.ttf",
    "SourceCodePro-Bold.ttf",
    "SourceCodePro-BoldItalic.ttf",
    // "SourceCodePro-ExtraBold.ttf",
    // "SourceCodePro-ExtraBoldItalic.ttf",
    // "SourceCodePro-Black.ttf",
    // "SourceCodePro-BlackItalic.ttf",
}

macro define($copy_over:ident; $( $filename:literal ),+ $(,)?) {
    // @Beacon @Task copy over LICENSES and COPYRIGHT notice
    pub(super) fn $copy_over(path: &std::path::Path) -> std::io::Result<()> {
        $(
            {
                let path = path.join($filename);
                if !path.exists() {
                    static FONT: &[u8] = include_bytes!(concat!("static/fonts/", $filename));
                    std::fs::write(path, FONT)?;
                }
            }
        )+

        Ok(())
    }
}
