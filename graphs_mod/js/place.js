function placeLegendAnnot() {
plot_height = $(".plot-container").outerHeight();
plot_width = $(".plot-container").outerWidth();
annotation_width = $(".annotation .cursor-pointer rect.bg").outerWidth();
legend_width = $(".legend rect.bg").outerWidth();
if ($(".gtitle").length == 1){document.querySelector(".gtitle").style.transform = "translate(0px,0px)"};
if ($(".annotation .cursor-pointer").length == 1){document.querySelector(".annotation .cursor-pointer").style.transform = "translate(" + (plot_width-annotation_width)/2 + "px," + (plot_height - 40) + "px)"};
if ($(".legend").length == 1){document.querySelector(".legend").style.transform = "translate(" + (plot_width-legend_width)/2 + "px," + (plot_height -50) + "px)"};
};
window.addEventListener("DOMContentLoaded", placeLegendAnnot, false);
