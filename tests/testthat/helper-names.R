local_name_repair_quiet <- function(frame = caller_env()) {
  local_options(rlib_name_repair_verbosity = "quiet", .frame = frame)
}
local_name_repair_verbose <- function(frame = caller_env()) {
  local_options(rlib_name_repair_verbosity = "verbose", .frame = frame)
}
