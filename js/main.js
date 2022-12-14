const STAGGER_DURATION = 2;
const PIN_DURATION = 3;
const TOTAL_DURATION = STAGGER_DURATION + PIN_DURATION;
const openModalButtons = document.querySelectorAll('[data-modal-target]')
const closeModalButtons = document.querySelectorAll('[data-close-button]')
const overlay = document.getElementById('overlay')

let screenHeight;
let screenWidth;


openModalButtons.forEach(button => {
  button.addEventListener('click', () => {
    const modal = document.querySelector(button.dataset.modalTarget)
    openModal(modal)
  })
})

overlay.addEventListener('click', () => {
  const modals = document.querySelectorAll('.modal.active')
  modals.forEach(modal => {
    closeModal(modal)
  })
})

closeModalButtons.forEach(button => {
  button.addEventListener('click', () => {
    const modal = button.closest('.modal')
    closeModal(modal)
  })
})

function openModal(modal) { //Open modal and close modal for the projet section popup 
  if (modal == null) return
  modal.classList.add('active')
  overlay.classList.add('active')
}

function closeModal(modal) {
  if (modal == null) return
  modal.classList.remove('active')
  overlay.classList.remove('active')
}

function isMobile() {
    if( /Android|webOS|iPhone|iPad|iPod|BlackBerry|IEMobile|Opera Mini/i.test(navigator.userAgent) ) {
        return true;
    }
    else {
        return false;
    }
}

function spaceOutSections(n) {
    let spacers = document.getElementsByClassName("spacer");
    for (spacer of spacers) {
        spacer.style.height = spacer.clientHeight*n + 'px';
    }
}

// For bouncable circle images with spinning borders
$(".icon-content").on("mouseenter click", function() {
    let parent = this.closest(".bouncable-border-rotatable");
    let iconBorder = parent.getElementsByClassName("icon-border")[0];
    
    let duration = 1.5;
    TweenMax.to(this, duration / 4, {y: -20, ease:Power1.easeOut});
    TweenMax.to(this, duration / 2, {y: 0, ease:Bounce.easeOut, delay:duration / 4});
    TweenMax.to(iconBorder, duration / 4, {y: -20, ease:Power1.easeOut});
    TweenMax.to(iconBorder, duration / 2, {y: 0, ease:Bounce.easeOut, delay:duration / 4});
    TweenMax.fromTo(iconBorder, duration, {rotation: 0, ease:Expo.easeIn}, {rotation:360, ease:Back.easeOut});
});

$(".item-icon").on("mouseenter click", function() {
    let duration = 1;
    TweenMax.to(this, duration / 4, {y:-40, ease:Power1.easeOut});
    TweenMax.to(this, duration / 2, {y:0, ease:Bounce.easeOut, delay:duration / 4});
    TweenMax.fromTo(this, 3/4 * duration, {rotation: 0}, {rotation:360, ease:Power3.easeOut});
})

$(".item-icon-small").on("mouseenter click", function() {
    let duration = 1;
    TweenMax.to(this, duration / 4, {y:-20, ease:Power1.easeOut});
    TweenMax.to(this, duration / 2, {y:0, ease:Bounce.easeOut, delay:duration / 4});
    TweenMax.fromTo(this, 3/4 * duration, {rotation: 0}, {rotation:360, ease:Power3.easeOut});
})
$('nav ul li a').click(function(){

    $('html').css("scrollBehavior", "smooth");
})

$(function() {

    // ------------------------------------------------ General setup --------------=-------------------------------- //
    // Rocket doesn't shake on mobile, only on web
    let canHover = !(matchMedia('(hover: none)').matches);
    if (canHover) {
        $('.scroll-top').addClass('can-hover');
    }

    screenHeight = $(window).height();
    screenWidth = $(window).width();
    if (isMobile()) {
        let ratio = window.devicePixelRatio || 1;
        screenHeight = window.innerHeight || $(window).height();
        screenWidth = screen.width * ratio;

        // vh does not work in mobile, so height is set dynamicaly
        $('.spacer').height(screenHeight);
        $('section').height(screenHeight);
        $('.full').height(screenHeight);
    }
    else {
        $('.scroll-top').addClass('can-hover');
    }

    if (!isMobile()) {
        let onlyMobile = document.getElementsByClassName("only-mobile");
        for (let i = 0; i < onlyMobile.length; i++) {
            onlyMobile[i].remove();
        }
    } else {
        let onlyWeb = document.getElementsByClassName("only-web");
        for (let i = 0; i < onlyWeb.length; i++) {
            onlyWeb[i].remove();
        }
    }

    // ---------------------------------------------- Rocket scrolling ---------------------------------------------- //
    jQuery(window).scroll(function(){
        if ($(window).scrollTop() > screenHeight) {
            $(".scroll-top").fadeIn();
        }
        else {
            $(".scroll-top").fadeOut();
            $(".scroll-top").removeClass("scroll-top_hover");
        }
    });
    $(".scroll-top").click(function() {
        $(window.opera ? 'html' : 'html, body').animate({
            scrollTop: 0
        }, "slow");
    });

    // ------------------------------------------ Body content animations ------------------------------------------- //

    let controller = new ScrollMagic.Controller();

    // Fade in the frontpage on for loading
    TweenMax.fromTo(".front", 2, {autoAlpha: 0}, {autoAlpha: 1})

    let sections = document.getElementsByClassName("section");
    let sectionTweens = [];

    sectionTweens.push(TweenMax.staggerFromTo(sections[0].getElementsByClassName("staggerAnimate"), 1, {y: screenHeight*1/2}, {y: 0, ease: Back.easeOut.config(0.75)}, 0.5));
    for (let i = 1; i < sections.length - 1; i++) {
        sectionTweens.push(TweenMax.staggerFromTo(sections[i].getElementsByClassName("staggerAnimate"), 1, {y: screenHeight*1/2}, {y: 0, ease: Back.easeOut.config(0.75)}, 0.5));
    }
    sectionTweens.push(TweenMax.fromTo(sections[sections.length - 1].getElementsByClassName("staggerAnimate"), 1, {autoAlpha: 0}, {autoAlpha: 1}));

    spaceOutSections(TOTAL_DURATION); // Set up spaces in between sections to account for scrolling
    staggerDuration = STAGGER_DURATION * screenHeight;
    pinDuration = PIN_DURATION * screenHeight;
    
    let sceneOffset = 0;
    let sceneOffsets = []

    for (let i = 0; i < sections.length; i++) {
        new ScrollMagic.Scene({
                triggerElement: "#trigger", 
                offset: sceneOffset + sections[i].clientHeight/2, 
                duration: staggerDuration
            })
            .setPin(sections[i])
            .setTween(sectionTweens[i])
            .addIndicators({name: `animation${i + 1}`})
            .addTo(controller)

        sceneOffsets.push({ "stagger": sceneOffset })
        sceneOffset += staggerDuration;
        
        let actualPinDuration = pinDuration;
        if (i == sections.length - 1) {
            actualPinDuration = 0;
        }
        new ScrollMagic.Scene({
                triggerElement: "#trigger", offset: sceneOffset + sections[i].clientHeight/2, duration: actualPinDuration
            })
            .setPin(sections[i])
            .addIndicators({name: `pin${i + 1}`})
            .addTo(controller)

        sceneOffsets[i].pause = sceneOffset;

        sceneOffset += pinDuration;
        sceneOffset += screenHeight;
    }

  });