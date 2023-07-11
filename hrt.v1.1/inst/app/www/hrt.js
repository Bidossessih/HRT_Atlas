
function changeButtonColor(val, id) {
    
    var elts = ["Nucleus", "Microtubules", "Target protein", "ER"]
    
    var myitem = val

    console.log("Item: ", myitem)

    if (elts.includes(myitem)) {
        var checkState = document.querySelector("#"+id).getAttribute("checked")

        if (checkState == "checked") {

            document.querySelector("#"+id).removeAttribute("checked");


        } else {
            document.querySelector("#"+id).setAttribute("checked", "checked");
        }


    }


};







/*Open result page based on selected cell type */

function disabledSearch() {

    if ($("#search").val() == "") {
        $("#buttonresult").addClass("disabled")

    }

    if ($("#search").val() != "") {
        $("#buttonresult").removeClass("disabled")
    }

};



function openResult() {

    var cell = $("#search").val().replaceAll(" ", "-")

    var specie = window.location.search.replace("?", "")
    window.location.href = "/?page=".concat(specie).concat("-housekeeping-gene&tissue=").concat(cell);
}


$(document).ready(function () {
    document.getElementById("search").addEventListener("change", disabledSearch);
    document.getElementById("buttonresult").addEventListener("click", openResult);

});



