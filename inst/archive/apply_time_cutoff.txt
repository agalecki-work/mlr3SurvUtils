# Test the function
if (require(survival) && require(data.table)) {
 library(survival)
  mgus2$etime <- with(mgus2, ifelse(pstat == 0, futime, ptime))
  mgus2$event <- factor(with(mgus2, ifelse(pstat == 0, 2 * death, 1)), 
                       0:2, 
                       labels = c("censor", "pcm", "death"))
  target_info <- c(id = "mgus2_etime", time = "etime", event = "event", type = "mstate")
  mgus2x <- apply_time_cutoff(mgus2, target_info, id = "id", time_cutoff = 500)
  print(head(mgus2x))
}
