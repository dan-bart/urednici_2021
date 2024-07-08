// Get the button
let mybutton = document.getElementById("scrollToNextBtn");

function scrollFunction() {
    if (document.body.scrollTop > 20 || document.documentElement.scrollTop > 20) {
        mybutton.style.display = "none";
    } else {
    	mybutton.style.display = "block";
    }
}

// When the user scrolls down 20px from the top of the document, hide the button
window.onscroll = function() {
    scrollFunction();
};

// When the user clicks on the button, scroll to the next graph
mybutton.onclick = function() {
    nextFunction();
};

function scrollToElement(selector) {
	$('html, body').animate({
	scrollTop: $(selector).offset().top
	}, 1000); // 1000 milliseconds = 1 second
}

function nextFunction() {
    scrollToElement('#graf_A1');
}