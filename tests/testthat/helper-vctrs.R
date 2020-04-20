
import_from <- function(ns, names, env = caller_env()) {
  objs <- env_get_list(ns_env(ns), names)
  env_bind(env, !!!objs)
}
