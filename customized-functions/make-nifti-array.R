library(oro.nifti)
library(tidyverse)
make_img_array <- function(file, dim = 3, labels = NULL) {
  nifti_file <- readNIfTI(file, reorient = FALSE)
  # Extract image data as an array
  img_data <- as.array(nifti_file)
  # Get voxel dimensions and origin (qform)
  voxel_dim <- nifti_file@pixdim[2:4]
  origin <- c(nifti_file@qoffset_x,nifti_file@qoffset_y, nifti_file@qoffset_z)
  
  if (dim==3) {
    # Create a grid of voxel indices
    xyz_grid <- expand.grid(
      x = 1:dim(img_data)[1],
      y = 1:dim(img_data)[2],
      z = 1:dim(img_data)[3]
    )
    # Extract voxel intensities
    xyz_grid$intensity <- as.vector(img_data)
    # Filter out zero-intensity voxels if needed
    xyz_grid <- xyz_grid %>% filter(intensity != 0)
    # Calculate MNI coordinates using voxel dimensions and origin
    xyz_grid <- xyz_grid %>%
      mutate(
        mni_x = (x - 1) * voxel_dim[1] - origin[1],
        mni_y = (y - 1) * voxel_dim[2] + origin[2],
        mni_z = (z - 1) * voxel_dim[3] + origin[3]
      ) %>%
      select(x,y,z, mni_x, mni_y, mni_z, intensity)
    if (is.null(labels)) {
      xyz_wide <- xyz_grid
    }else {
      voxel.labels <- read_rds(labels)
      xyz_wide <- inner_join(voxel.labels, xyz_grid)
    }
    return(xyz_wide)
  } else if(dim==4) {
    xyzzy_grid <- expand.grid(
      x = 1:dim(img_data)[1],
      y = 1:dim(img_data)[2],
      z = 1:dim(img_data)[3],
      t = 1:dim(img_data)[4]
    )
    # Extract intensity values per frame
    xyzzy_grid$intensity <- as.vector(img_data)
    xyzzy_grid <- xyzzy_grid %>%
      mutate(
        mni_x = (x - 1) * voxel_dim[1] - origin[1],
        mni_y = (y - 1) * voxel_dim[2] + origin[2],
        mni_z = (z - 1) * voxel_dim[3] + origin[3]
      ) %>%
      select(x,y,z, mni_x, mni_y, mni_z, t, intensity)
    gc()
    voxel.labels <- read_rds(labels)
    xyzzy_wide <- inner_join(voxel.labels, xyzzy_grid) %>%
      pivot_wider(
        id_cols = c(colnames(voxel.labels)),
        names_from = t,
        values_from = intensity,
        names_prefix = "Frame_"
      )
    return(xyzzy_wide)
  }
}
