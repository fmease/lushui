mod nat32 {
    #[unsafe(export_name = "nat32.successor")]
    extern "C" fn successor(x: u32) -> u32 {
        x + 1
    }
}
