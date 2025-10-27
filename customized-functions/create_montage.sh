#!/bin/bash

# Function to create overlay images and generate montage
create_montage() {
  local bottom_image=$1
  local top_image=$2
  local output_image=$3

  # Temporary directory for intermediate files
  tmp_dir=$(mktemp -d)
  
  # Overlay images with transparency (50% here, adjust as needed)
  overlay 1 0 ${bottom_image} -a ${top_image} 0 0.3 ${tmp_dir}/overlayed.nii.gz s

  # Generate slices at specified x positions
  step_size=12
  num_slices=30
  for i in $(seq 0 $((num_slices - 1))); do
    x_pos=$((i * step_size))
    slicer ${tmp_dir}/overlayed.nii.gz -x ${x_pos} ${tmp_dir}/slice_${i}.png
  done
  
  # Split the slices into individual files
  convert ${tmp_dir}/slice.png -crop 1x6@ +repage +adjoin ${tmp_dir}/slice_%d.png

  # Create a montage (5 columns x 6 rows)
  montage ${tmp_dir}/slice_*.png -tile 5x6 -geometry +1+1 ${output_image}
  
  # Clean up temporary directory
  rm -r ${tmp_dir}
}

# Directory containing your images
input_dir="path_to_your_nifti_images"
output_dir="path_to_save_png_images"

# Iterate over subjects
for subject_dir in ${input_dir}/*; do
  subject=$(basename ${subject_dir})
  bottom_image="${subject_dir}/T1.nii.gz"  # Change to your actual file names
  top_image="${subject_dir}/MNI.nii.gz"    # Change to your actual file names
  output_image="${output_dir}/${subject}_overlay.png"
  
  create_montage ${bottom_image} ${top_image} ${output_image}
done



make3Dpng \
    --bg ${MNI_W} \
      --bg-color "#000000,#00FF00,#FFFFFF" \
      --bg-thresh "2.5,97.5" \
    --fg ${sub_T1} \
      --fg-thresh "2.5,97.5" \
      --fg-color "#000000,#FF00FF,#FFFFFF" \
      --fg-alpha 50 \
      --fg-cbar "false"\
    --layout "9:x;9:x;9:x;9:y;9:y;9:y;9:z;9:z;9:z" \
    --offset "0,0,0" \
    --filename ${participant_id}_reg-overlay \
    --dir-save ${sub_DIR}


for sub_dir in /Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/mri/data/derivatives/anat-registration/*; do
  if [[ -d ${sub_dir} ]]; then
    # Extract participant ID from the sub-folder name
    participant_id=$(basename ${sub_dir})
    echo ${participant_id}

    # Define the output directory (can be same as sub_dir or another directory)
    output_dir="${sub_dir}"
    echo ${output_dir}

    # Apply the make3Dpng function
    make3Dpng \
      --bg ${MNI_W} \
      --bg-color "#000000,#00FF00,#FFFFFF" \
      --bg-thresh "2.5,97.5" \
      --fg ${sub_dir}/sub-${participant_id}-T1-to-MNI_Warped.nii.gz \
      --fg-thresh "2.5,97.5" \
      --fg-color "#000000,#FF00FF,#FFFFFF" \
      --fg-alpha 50 \
      --fg-cbar "false" \
      --layout "9:x;9:x;9:x;9:y;9:y;9:y;9:z;9:z;9:z" \
      --offset "0,0,0" \
      --filename ${participant_id}_reg-overlay \
      --dir-save ${output_dir}
  fi
done
