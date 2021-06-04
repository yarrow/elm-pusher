import elmPlugin from 'rollup-plugin-elm'

export default {
  root: 'public',
  plugins: [elmPlugin()],
  vite: {
      optimizeDeps: {
          include: [ 'node_modules/uuid/dist' ]
      }
  }
}
