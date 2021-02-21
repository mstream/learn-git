module.exports = {
  purge: [],
  darkMode: false,
  theme: {
    extend: {},
    namedGroups: ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"],
  },
  variants: {
    extend: {},
  },
  plugins: [
    require("@tailwindcss/aspect-ratio"),
    require("@tailwindcss/forms"),
    require("@tailwindcss/line-clamp"),
    require("@tailwindcss/typography"),
    require("tailwindcss-named-groups"),
  ],
}
