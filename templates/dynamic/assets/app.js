let currentSortedIndex = 0;
let currentSortAsc = true;
const currentFilters = {};
const table = document.querySelector("table");
const rows = Array.from(table.rows).splice(1); // Skip header row
const sortables = document.querySelectorAll('th[data-sortable]');
const filters = document.querySelectorAll('[data-filter]');

function sortTable(index, ascending, type) {
    const compareFn = (a, b) => {
        let x = a.cells[index].textContent.trim();
        let y = b.cells[index].textContent.trim();

        if (type === 'str') {
            x = x.toLowerCase();
            y = y.toLowerCase();
            return ascending ? x.localeCompare(y) : y.localeCompare(x);
        } else {
            x = parseFloat(x);
            y = parseFloat(y);
            return ascending ? x - y : y - x;
        }
    };
    rows.sort(compareFn).forEach(row => table.tBodies[0].appendChild(row));
}

sortables.forEach((header, index) => {
    header.addEventListener("click", () => {
        currentSortAsc=(currentSortedIndex === index) ? !currentSortAsc : true;
        sortTable(index, currentSortAsc, header.getAttribute('data-sortable'));
        sortables[currentSortedIndex].classList.remove("asc", "dsc");
        sortables[index].classList.add(currentSortAsc ? "asc" : "dsc");
        currentSortedIndex = index;
    });
});

filters.forEach((f) => {
    const target = parseInt(f.getAttribute('data-filter'), 10);
    const zeroCompare = f.getAttribute('data-filter-notzero');

    function applyFilters() {
        rows.forEach(row => {
            const isHidden = Object.entries(currentFilters).some(
            ([colIndex, filterValue]) => {
                const cellValue = row.cells[colIndex].textContent.trim();
                if (typeof filterValue === "boolean") {
                    return filterValue && (
                        cellValue == "" || parseFloat(cellValue) === 0);
                }
                return !cellValue.toLowerCase().includes(
                    filterValue.toLowerCase());
            });
            row.style.display = isHidden ? 'none' : '';
        });
    }

    f.addEventListener('input', event => {
        currentFilters[target] = event.target.type === 'checkbox' ?
            event.target.checked : event.target.value;
        applyFilters();
    });

});
