pub struct AncillaryArtifactBuilder {
    uploaders: Vec<Arc<dyn ArtifactUploader>>,
}

impl AncillaryArtifactBuilder {
    pub fn new(uploaders: Vec<Arc<dyn ArtifactUploader>>) -> Self {
        Self { uploaders }
    }

    fn create_archive(&self, db_directory: &Path) -> StdResult<PathBuf> {
        todo!()
    }

    pub async fn create_and_upload_archive(
        &self,
        db_directory: &Path,
    ) -> StdResult<Vec<AncillaryLocation>> {
        let archive_path = self.create_archive(db_directory)?;

        let mut locations = Vec::new();
        // for uploader in &self.uploaders {
        //     let location = uploader.upload(archive_path.clone())?;
        //     if let ArtifactLocation::Ancillary(ancillary_location) = location {
        //         locations.push(ancillary_location);
        //     }
        // }

        Ok(locations)
    }
}
