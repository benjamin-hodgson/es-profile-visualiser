$(document).ready(() => {
    $("li").click(e => {
        e.stopPropagation();
        $(e.currentTarget).toggleClass("hidden-children");
    });
});