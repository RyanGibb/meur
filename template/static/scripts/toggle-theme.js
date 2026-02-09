const themeToggle = document.getElementById('theme-toggle');
const htmlElement = document.documentElement;
const prefersDarkScheme = window.matchMedia("(prefers-color-scheme: dark)");
const favicon = document.getElementById('favicon');

function setTheme(theme) {
    htmlElement.setAttribute('data-theme', theme);
    localStorage.setItem('theme', theme);
    if (theme === 'dark') {
        themeToggle.setAttribute('title', 'Switch to light mode');
        if (favicon) {
            favicon.href = "/favicon_dark.svg";
        }
    } else {
        themeToggle.setAttribute('title', 'Switch to dark mode');
        if (favicon) {
            favicon.href = "/favicon_light.svg";
        }
    }
}

// Checks localStorage first, then OS preference, then defaults to light
function getCurrentTheme() {
    const savedTheme = localStorage.getItem('theme');
    if (savedTheme) {
        return savedTheme;
    }
    if (prefersDarkScheme.matches) {
         return 'dark';
    }
    return 'light';
}

// Set the initial theme on page load
const initialTheme = getCurrentTheme();
setTheme(initialTheme);

// Add event listener to the toggle button
themeToggle.addEventListener('click', () => {
    const currentTheme = htmlElement.getAttribute('data-theme');
    const newTheme = currentTheme === 'dark' ? 'light' : 'dark';
    setTheme(newTheme);
});

prefersDarkScheme.addEventListener('change', (e) => {
    // Only change if no theme has been manually set via localStorage
    if (!localStorage.getItem('theme')) {
        setTheme(e.matches ? 'dark' : 'light');
    }
});
