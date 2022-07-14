use lushui_span::{ByteIndex, LocalByteIndex, SourceMap, Span};
use std::path::Path;
use tower_lsp::lsp_types::{Location, Position, Range, Url};

pub(crate) trait ToLocationExt {
    fn to_location(self, map: &SourceMap) -> Location;
}

impl ToLocationExt for Span {
    fn to_location(self, map: &SourceMap) -> Location {
        let lines = map.lines_with_highlight(self);

        Location {
            // @Beacon @Task handle anonymous SourceFiles smh!!!
            uri: Url::from_file_path(lines.path.unwrap()).unwrap(),
            range: Range {
                start: Position {
                    line: lines.first.number - 1,
                    character: lines.first.highlight.start - 1,
                },
                end: match lines.last {
                    Some(line) => Position {
                        line: line.number - 1,
                        character: line.highlight.end - 1,
                    },
                    None => Position {
                        line: lines.first.number - 1,
                        character: lines.first.highlight.end - 1,
                    },
                },
            },
        }
    }
}

pub(crate) trait FromPositionExt {
    fn from_position(position: Position, path: &Path, map: &SourceMap) -> Self;
}

impl FromPositionExt for ByteIndex {
    // @Beacon @Note this is an abomination!!!
    fn from_position(position: Position, path: &Path, map: &SourceMap) -> Self {
        let file = map.file_by_path(path).unwrap();
        let mut index = LocalByteIndex::new(0);

        #[allow(clippy::map_unwrap_or)] // @Temporary
        for (line_number, line) in file.content().split('\n').enumerate() {
            if line_number == position.line as usize {
                index += line
                    .char_indices()
                    .enumerate()
                    .find(|&(character_index, _)| character_index == position.character as usize)
                    .map(|(_, (byte_index, _))| u32::try_from(byte_index).unwrap())
                    // @Temporary just a hack, should be unwrap() I think
                    .unwrap_or_else(|| u32::try_from(line.len()).unwrap());
                break;
            }

            index += u32::try_from(line.len()).unwrap() + 1;
        }

        index.global(file)
    }
}

// @Task write tests
