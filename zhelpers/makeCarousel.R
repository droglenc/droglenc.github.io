#' @title: Make a carousel of all images in a folder
#' @param id Name for the carousel (make unique if more than one carousel on a page).
#' @param path A relative path to the folder with the images. The folder should only contain images that will form the carousel.
#' @param width The relative width of the carousel (can only be 100, 75, or 50%).
#' @param captions Logical for whether captions should be added. Captions are the image file names sans extension.
#' @param controls Logical for whether the Previous and Next control items should be used.
#' @param indicators Logical for whether indicator buttons should be shown for the image list.
#' @param interval The amount of time between images on the carousel.
#' @param fade Logical for whether there is a slide (\code{FALSE}) or fade between images.
#' @param hover Logical for whether the carousel should stop on a mouseover.

makeCarousel <- function(id,path,imgs=NULL,caps=NULL,
                         width=c("100","75","50"),center=TRUE,
                         captions=TRUE,controls=TRUE,indicators=FALSE,
                         interval=2500,fade=FALSE,hover=TRUE) {

  ## If imgs is NULL, use all files in path folder, otherwise given in imgs
  if (is.null(imgs)) imgs <- list.files(path=path)
  ## If caps is NULL, use filename as caps, otherwise given in caps
  if (is.null(caps)) caps <- tools::file_path_sans_ext(imgs)
  ## Make sure imgs and caps is same length
  if (length(imgs)!=length(caps)) stop("'imgs' and 'caps' not same length!",call.=FALSE)

  ## Get the width for the carousel container
  width <- as.character(width)
  width <- match.arg(width)

    ## start the carousel container
  tmp <- paste0("<div id='carousel",id,"Captions' class='carousel slide",ifelse(fade," carousel-fade","")," w-",width,"' data-bs-ride='carousel'>","\n")

  ## create the indicators/buttons (if asked for)
  if (indicators) {
    tmp <- paste0(tmp,"<div class='carousel-indicators'>","\n")
    for (i in seq_along(imgs)) {
      tmp <- paste0(tmp,"<button type='button' data-bs-target='#carousel",id,"Captions' data-bs-slide-to='",i-1,"' class='active' aria-current='true' aria-label='Slide ",i,"'></button>","\n")
    }
    tmp <- paste0(tmp,"</div>","\n")
  }

  ## add the images for each slide
  tmp <- paste0(tmp,"<div class='carousel-inner'>","\n")
  for (i in seq_along(imgs)) {
    if (i==1) tmp <- paste0(tmp,"<div class='carousel-item active' data-bs-pause='hover' data-bs-interval='",interval,"'>","\n")
    else tmp <- paste0(tmp,"<div class='carousel-item' data-bs-pause='hover' data-bs-interval='",interval,"'>","\n")
    tmp <- paste0(tmp,"<img src='",path,imgs[i],"' class='d-block w-100' alt='",caps[i],"'>","\n")
    ## add captions (if asked for)
    if (captions) {
      tmp <- paste0(tmp,"<div class='carousel-caption d-none d-md-block'>","\n")
      tmp <- paste0(tmp,"<h5>",caps[i],"</h5>","\n")
      tmp <- paste0(tmp,"</div>","\n")
    }
    tmp <- paste0(tmp,"</div>","\n")
  }
  tmp <- paste0(tmp,"</div>","\n")

  ## add controls to the slide (if asked for)
  if (controls) {
    tmp <- paste0(tmp,"<button class='carousel-control-prev' type='button' data-bs-target='#carousel",id,"Captions' data-bs-slide='prev'>","\n")
    tmp <- paste0(tmp,"<span class='carousel-control-prev-icon' aria-hidden='true'></span>","\n")
    tmp <- paste0(tmp,"<span class='visually-hidden'>Previous</span>","\n")
    tmp <- paste0(tmp,'</button>',"\n")
    tmp <- paste0(tmp,"<button class='carousel-control-next' type='button' data-bs-target='#carousel",id,"Captions' data-bs-slide='next'>","\n")
    tmp <- paste0(tmp,"<span class='carousel-control-next-icon' aria-hidden='true'></span>","\n")
    tmp <- paste0(tmp,"<span class='visually-hidden'>Next</span>","\n")
    tmp <- paste0(tmp,'</button>',"\n")
  }

  ## finish up
  tmp <- paste0(tmp,"</div>","\n")
  ## center if asked for
  if (center) tmp <- paste0("<center>\n",tmp,"</center>\n")
  ## display the object
  cat(tmp)
}
