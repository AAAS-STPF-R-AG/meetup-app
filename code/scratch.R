# scratch code 

devtools::install_github("rladies/meetupr")

meetupr::find_groups("data science")

install.packages("meetupapi")


meetupapi:::.construct_req(method = "find/upcoming_events", key = NA)

meetupr::get_events("rladies-dc")

find_upcoming <- function (urlname, event_status = "upcoming", api_key = NULL) {
  if (!is.null(event_status) && !event_status %in% c("cancelled", 
                                                     "draft", "past", "proposed", "suggested", 
                                                     "upcoming")) {
    stop(sprintf("Event status %s not allowed", event_status))
  }
  if (length(event_status) > 1) {
    event_status <- paste(event_status, collapse = ",")
  }
  api_method <- "find/upcoming_events"
  res <- meetupr:::.fetch_results(api_method, api_key, event_status)
  tibble::tibble(id = purrr::map_chr(res, "id"), 
                 name = purrr::map_chr(res, "name"), 
                 created = .date_helper(purrr::map_dbl(res,  "created", .default = NA)),
                 status = purrr::map_chr(res, "status", .default = NA), 
                 time = .date_helper(purrr::map_dbl(res, "time", .default = NA)), 
                 local_date = as.Date(purrr::map_chr(res, "local_date", .default = NA)), 
                 local_time = purrr::map_chr(res, "local_time", .default = NA), 
                 waitlist_count = purrr::map_int(res, "waitlist_count"), 
                 yes_rsvp_count = purrr::map_int(res,  "yes_rsvp_count"), 
                 venue_id = purrr::map_int(res, c("venue", "id"), .default = NA), 
                 venue_name = purrr::map_chr(res, c("venue", "name"), .default = NA), 
                 venue_lat = purrr::map_dbl(res, c("venue", "lat"), .default = NA), 
                 venue_lon = purrr::map_dbl(res, c("venue", "lon"), .default = NA), 
                 venue_address_1 = purrr::map_chr(res, c("venue", "address_1"), .default = NA), 
                 venue_city = purrr::map_chr(res, c("venue", "city"), .default = NA), 
                 venue_state = purrr::map_chr(res, c("venue", "state"), .default = NA), 
                 venue_zip = purrr::map_chr(res, c("venue", "zip"),  .default = NA), 
                 venue_country = purrr::map_chr(res, c("venue", "country"), .default = NA), 
                 description = purrr::map_chr(res, c("description"), .default = NA), 
                 link = purrr::map_chr(res, c("link")), 
                 resource = res)
}

find_upcoming

paste(readLines("dat/upcoming_res.json"), collapse = "\n") %>% tidyjson::gather_array()



test <- jsonlite::read_json("dat/upcoming_res.json", simplifyVector = T)

head(test[,1:6])

.fetch_results(api_method, api_key, event_status)

# event brite 

devtools::install_github("paddytobias/eventbriteR")

eventbriteR::eb_query(token = Sys.getenv("EVENTBRITE_API_KEY"))