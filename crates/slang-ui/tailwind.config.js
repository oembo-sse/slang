/** @type {import('tailwindcss').Config} */
export default {
  content: ["./static/*.{html,js}"],
  theme: {
    extend: {},
  },
  plugins: [
    require('@tailwindcss/forms'),
  ],
};
