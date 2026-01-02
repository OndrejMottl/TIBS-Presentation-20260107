generate_qr_code <- function(url,
                             name,
                             foreground_color = "black",
                             background_color = "white",
                             plot = TRUE) {
  # Generate QR code object
  qr_obj <- 
    qrcode::qr_code(
      x = url,
      ecl = "H")

  # Generate SVG file
  qrcode::generate_svg(
    qrcode = qr_obj,
    filename = here::here("Presentation/Materials/QR", paste0("qr_", name, ".svg")),
    foreground = colours[[foreground_color]],
    background = colours[[background_color]],
    show = FALSE
  )

  # Optionally plot the QR code
  if (
    isTRUE(plot)
    ) {
    include_local_figure(
      data_source = paste0("QR/qr_", name, ".svg"))
  }
}
