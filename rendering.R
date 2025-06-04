# Render the animation as PNG frames

dir.create(frames_dir, showWarnings = FALSE)

animate(
  p,
  duration = video_length + 2, # +2 is for the pauses at the start and beginning of the video
  start_pause = framerate,
  end_pause = framerate,
  fps = framerate,
  width = ifelse(format == "landscape", 1920, 1080), 
  height = 1080,
  res = 100,
  device = "ragg_png",
  renderer = file_renderer(frames_dir, prefix = "frame_", overwrite = TRUE)
)

# Use magick to read the PNG logo and composite it behind each frame

logo <- image_read(image)

frames <- list.files(frames_dir, pattern = "^frame_.*\\.png$", full.names = TRUE) # List all rendered frames

dir.create(composited_dir, showWarnings = FALSE)

for (f in frames) {
  frame <- image_read(f)
  
  # Composite the logo over the frame
  composited <- image_composite(frame, logo, operator = "Over")
  
  # Save the result
  out_file <- file.path(composited_dir, basename(f))
  image_write(composited, out_file)
}

# Convert the final PNG frames to a video

av::av_encode_video(
  list.files(composited_dir, full.names = TRUE),
  output = output_path,
  framerate = framerate,
  vfilter = "format=yuv420p"
)

utils::browseURL(output_path) # Open the video just created

unlink(c(frames_dir, composited_dir), recursive = TRUE) # Delete temporary folders containing frames

gc() # Free unused memory