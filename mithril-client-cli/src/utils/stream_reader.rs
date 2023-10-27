use flume::Receiver;
use std::io;

// Credits and many thanks to https://stackoverflow.com/a/69967522 for most of this code

/// A channel receiver that implement [io::Read].
pub struct StreamReader {
    receiver: Receiver<Vec<u8>>,
    current: io::Cursor<Vec<u8>>,
}

impl StreamReader {
    /// [StreamReader] factory
    pub fn new(receiver: Receiver<Vec<u8>>) -> Self {
        Self {
            receiver,
            current: io::Cursor::new(vec![]),
        }
    }
}

impl io::Read for StreamReader {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        if self.current.position() == self.current.get_ref().len() as u64 {
            // We've exhausted the previous chunk, get a new one.
            if let Ok(vec) = self.receiver.recv() {
                self.current = io::Cursor::new(vec);
            }
            // If recv() "fails", it means the sender closed its part of
            // the channel, which means EOF. Propagate EOF by allowing
            // a read from the exhausted cursor.
        }
        self.current.read(buf)
    }
}
