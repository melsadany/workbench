make_nifti_from_df <- function(df, ref="/Dedicated/jmichaelson-wdata/msmuhammad/refs/fsl-data/standard/MNI152_T1_2mm_brain.nii.gz") {
  library(oro.nifti)
  # 1. Read template image
  template_nifti <- readNIfTI(ref, reorient = FALSE)
  # 2. Ensure float storage
  template_nifti@"datatype" <- 16  # FLOAT32
  template_nifti@"bitpix" <- 32
  # 3. Zero out all intensities
  template_nifti[] <- 0
  # 4. Bounds check (optional but recommended)
  dims <- dim(template_nifti)
  df <- subset(df, x >= 1 & x <= dims[1] &
                 y >= 1 & y <= dims[2] &
                 z >= 1 & z <= dims[3])
  # 5. Convert 3D indices to linear indices
  linear_map <- function(x, y, z) {
    return((z - 1) * dims[1] * dims[2] + (y - 1) * dims[1] + x)
  }
  lin_idx <- with(df, linear_map(x, y, z))
  # 6. Assign intensity values directly
  img_vector <- as.vector(template_nifti)
  img_vector[lin_idx] <- df$intensity
  template_nifti[] <- img_vector
  # 6. Return result
  return(template_nifti)
}
